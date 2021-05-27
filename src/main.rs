use anyhow::Result;
use clap::Clap;
use serde::Serialize;
use std::io::Write;
use std::process::Output;
use std::rc::Rc;

#[derive(Debug, thiserror::Error)]
#[error("unknown format({name})")]
struct UnknownOutputFormat {
    name: String,
}

#[derive(Clap, Debug)]
#[clap(version = env!("CARGO_PKG_VERSION"), author = "itn3000")]
struct FindOption {
    #[clap(name = "BASEPATH", about = "base path", default_value = ".")]
    basepath: Vec<String>,
    #[clap(short, long, about = "include glob pattern(default: '**/*')")]
    include: Vec<String>,
    #[clap(short, long, about = "exclude glob pattern(default: empty)")]
    exclude: Vec<String>,
    #[clap(short, long, about = "output file path(default: stdout)")]
    output: Option<String>,
    #[clap(long, about = "follow symlink(default: false)")]
    follow_symlink: bool,
    #[clap(short, long, about = "max depth", default_value = "100")]
    max_depth: String,
    #[clap(
        long,
        about = "output format(csv or ndjson listpath is valid)",
        default_value = "csv"
    )]
    output_format: String,
    #[clap(long, about = "list only files or symlink")]
    leaf_only: bool,
    #[clap(long, about = "ignore case in pattern matching")]
    ignore_case: bool,
    #[clap(long, about = "list only directory")]
    dir_only: bool,
    #[clap(long, about = "output total size of directory(bytes)")]
    total_size: bool,
}

enum RecordWriter<T>
where
    T: std::io::Write,
{
    Csv(csv::Writer<T>),
    NdJson(OutputStream),
    ListPath(OutputStream),
}

impl<T> RecordWriter<T>
where
    T: std::io::Write,
{
    pub fn write_record(&mut self, record: FileRecord) -> Result<()> {
        match self {
            Self::Csv(w) => {
                w.write_record(&[
                    record.path.as_str(),
                    record.link_target.unwrap_or(String::new()).as_str(),
                    record.file_type.as_str(),
                    record
                        .length
                        .as_ref()
                        .map(|x| format!("{}", x))
                        .unwrap_or(String::new())
                        .as_str(),
                    record
                        .last_modified
                        .as_ref()
                        .unwrap_or(&String::new())
                        .as_str(),
                ])?;
            }
            Self::NdJson(v) => {
                let jsonstr = serde_json::to_string(&record)?;
                v.write(jsonstr.as_bytes())?;
                v.write(b"\n")?;
            },
            Self::ListPath(v) => {
                v.write(record.path.as_bytes())?;
                v.write(b"\n")?;
            }
        }
        Ok(())
    }
    pub fn output_header(&mut self) -> Result<()> {
        match self {
            Self::Csv(w) => {
                w.write_record(&[
                    "path",
                    "link_target",
                    "file_type",
                    "length",
                    "last_modified",
                ])?;
            }
            Self::NdJson(_) => {},
            Self::ListPath(_) => {},
        };
        Ok(())
    }
}

#[derive(Serialize)]
struct FileRecord {
    path: String,
    length: Option<u64>,
    file_type: String,
    last_modified: Option<String>,
    link_target: Option<String>,
}

enum OutputStream {
    Stdout(std::io::Stdout),
    File(std::fs::File),
}

impl Write for OutputStream {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Self::Stdout(v) => v.write(buf),
            Self::File(v) => v.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Self::Stdout(v) => v.flush(),
            Self::File(v) => v.flush(),
        }
    }
}

struct FindContext<'a> {
    path: std::path::PathBuf,
    include: Rc<Vec<glob::Pattern>>,
    exclude: Rc<Vec<glob::Pattern>>,
    leaf_only: bool,
    output_stream: &'a mut RecordWriter<OutputStream>,
    follow_symlink: bool,
    max_depth: i32,
    match_option: glob::MatchOptions,
    dir_only: bool,
    output_total: bool,
}

fn create_record_writer(
    output_path: Option<&str>,
    format: &str,
) -> Result<RecordWriter<OutputStream>> {
    let ost = if let Some(output_path) = output_path {
        let f = std::fs::File::create(output_path)?;
        OutputStream::File(f)
    } else {
        OutputStream::Stdout(std::io::stdout())
    };
    let ret = match format.to_lowercase().as_str() {
        "csv" => Ok(RecordWriter::Csv(csv::Writer::from_writer(ost))),
        "ndjson" => Ok(RecordWriter::NdJson(ost)),
        "listpath" => Ok(RecordWriter::ListPath(ost)),
        s => Err(anyhow::Error::from(UnknownOutputFormat {
            name: s.to_owned(),
        })),
    }?;
    Ok(ret)
}

impl<'a> FindContext<'a> {
    pub fn from_options(
        opts: &'a FindOption,
        w: &'a mut RecordWriter<OutputStream>,
        base_path: &str,
    ) -> Result<Self> {
        let includes: Result<Vec<glob::Pattern>, glob::PatternError> = if opts.include.len() > 0 {
            opts.include.iter().map(|x| glob::Pattern::new(x)).collect()
        } else {
            let pattern = glob::Pattern::new("**/*")?;
            Ok(vec![pattern])
        };
        let excludes: Result<Vec<glob::Pattern>, glob::PatternError> =
            opts.exclude.iter().map(|x| glob::Pattern::new(x)).collect();
        let p = std::path::PathBuf::from(base_path);
        let mut match_option = glob::MatchOptions::new();
        match_option.case_sensitive = !opts.ignore_case;
        let max_depth = opts.max_depth.parse::<i32>()?;
        let output_total = opts.total_size;
        let dir_only = opts.dir_only;
        Ok(FindContext {
            include: Rc::new(includes?),
            exclude: Rc::new(excludes?),
            leaf_only: opts.leaf_only,
            path: p,
            output_stream: w,
            follow_symlink: opts.follow_symlink,
            max_depth: max_depth,
            match_option: match_option,
            output_total: output_total,
            dir_only: dir_only,
        })
    }
    pub fn with_path(mut self, new_path: &std::path::Path) -> Self {
        self.path = std::path::PathBuf::from(new_path);
        self
    }
}

fn output_symlink_info<'a>(
    mut ctx: FindContext<'a>,
    path: &std::path::Path,
    link_target: &str,
    symlink_meta: Option<&std::fs::Metadata>,
    target_meta: Option<std::fs::Metadata>
) -> Result<(FindContext<'a>, u64)> {
    if !check_include_exclude_path(path, &ctx.include, &ctx.exclude, &ctx.match_option) {
        return Ok((ctx, 0));
    }
    if !ctx
        .include
        .iter()
        .any(|x| x.matches_with(path.to_string_lossy().as_ref(), ctx.match_option.clone()))
    {
        return Ok((ctx, 0));
    }
    let len = if let Some(target_meta) = target_meta {
        target_meta.len()
    } else {
        0
    };
    let modified = if let Some(m) = symlink_meta {
        match m.modified() {
            Ok(v) => Some(v),
            Err(_) => None
        }
    } else {
        None
    };
    if !ctx.dir_only {
        write_record(
            &mut ctx.output_stream,
            path.to_string_lossy().as_ref(),
            Some(link_target),
            "link",
            Some(len),
            modified,
        )?;
    }
    Ok((ctx, len))
}

fn output_file_info<'a>(
    mut ctx: FindContext<'a>,
    path: &std::path::Path,
    meta: Option<std::fs::Metadata>,
) -> Result<(FindContext<'a>, u64)> {
    if !check_include_exclude_path(path, &ctx.include, &ctx.exclude, &ctx.match_option) {
        return Ok((ctx, 0));
    }
    let parent = ctx.path.clone();
    if !ctx
        .include
        .iter()
        .any(|x| x.matches_with(path.to_string_lossy().as_ref(), ctx.match_option.clone()))
    {
        return Ok((ctx, 0));
    }
    let (len, modified) = if let Some(meta) = meta {
        (Some(meta.len()), Some(meta.modified()))
    } else {
        (None, None)
    };
    let modified = if let Some(m) = modified {
        match m {
            Ok(v) => Some(v),
            Err(e) => {
                eprintln!(
                    "failed to transform modified to datetime({}): {:?}",
                    path.to_string_lossy(),
                    e
                );
                None
            }
        }
    } else {
        None
    };
    if !ctx.dir_only {
        write_record(
            &mut ctx.output_stream,
            path.to_string_lossy().as_ref(),
            None,
            "file",
            len,
            modified,
        )?;
    }
    Ok((ctx.with_path(parent.as_path()), len.unwrap_or(0)))
}

fn output_file_info_dentry<'a>(
    ctx: FindContext<'a>,
    dentry: &std::fs::DirEntry,
) -> Result<(FindContext<'a>, u64)> {
    let path = dentry.path();
    let meta = match dentry.metadata() {
        Ok(v) => Some(v),
        Err(e) => {
            eprintln!(
                "failed to get metadata for {}: {:?}",
                path.to_string_lossy(),
                e
            );
            None
        }
    };
    output_file_info(ctx, path.as_path(), meta)
}

fn write_record<T>(
    writer: &mut RecordWriter<T>,
    path: &str,
    link_target: Option<&str>,
    file_type: &str,
    length: Option<u64>,
    last_write: Option<std::time::SystemTime>,
) -> Result<()>
where
    T: std::io::Write,
{
    let ststr = match last_write {
        Some(v) => {
            let dt = chrono::DateTime::<chrono::Local>::from(v);
            dt.format("%Y-%m-%d %H:%M:%S").to_string()
        }
        None => String::new(),
    };
    writer.write_record(FileRecord {
        path: path.to_owned(),
        link_target: link_target.map(|x| x.to_owned()),
        file_type: file_type.to_owned(),
        length: length,
        last_modified: Some(ststr),
    })?;
    Ok(())
}

fn check_include_exclude(s: &str, includes: &[glob::Pattern], excludes: &[glob::Pattern], match_option: &glob::MatchOptions) -> bool {
    let match_option = match_option.clone();
    if excludes.iter().any(|x| x.matches_with(s, match_option)) {
        false
    } else if includes.iter().any(|x| x.matches_with(s, match_option)) {
        true
    } else {
        false
    }
}

fn check_include_exclude_path(
    p: &std::path::Path,
    includes: &[glob::Pattern],
    excludes: &[glob::Pattern],
    match_option: &glob::MatchOptions,
) -> bool {
    check_include_exclude(p.to_string_lossy().as_ref(), includes, excludes, match_option)
}

fn retrieve_symlink<'a>(
    mut ctx: FindContext<'a>,
    parent: &std::path::Path,
    meta: Option<std::fs::Metadata>,
    depth: i32,
) -> Result<(FindContext<'a>, u64)> {
    if depth >= ctx.max_depth {
        return Ok((ctx.with_path(parent), 0));
    }
    let path = ctx.path.clone();
    let link_target = match std::fs::read_link(path.clone()) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("failed to readlink({}): {:?}", path.to_string_lossy(), e);
            return Ok((ctx.with_path(parent), 0));
        }
    };
    if ctx.follow_symlink {
        let link_path = if link_target.as_path().is_relative() {
            if let Some(p) = path.parent() {
                p.join(link_target.clone())
            } else {
                link_target.clone()
            }
        } else {
            link_target.clone()
        };
        match std::fs::metadata(link_path.clone()) {
            Ok(v) => {
                let ftype = v.file_type();
                if ftype.is_symlink() {
                    ctx = ctx.with_path(link_path.as_path());
                    return retrieve_symlink(ctx, parent, Some(v), depth + 1);
                }
                if ftype.is_file() {
                    let (x, y) = output_file_info(ctx, link_path.as_path(), Some(v))?;
                    ctx = x;
                    return Ok((ctx.with_path(parent), y));
                } else if ftype.is_dir() {
                    ctx = ctx.with_path(link_path.as_path());
                    let modified = if let Some(meta) = meta {
                        meta.modified().map(|x| Some(x)).unwrap_or(None)
                    } else {
                        None
                    };
                    if check_include_exclude_path(&path, &ctx.include, &ctx.exclude, &ctx.match_option) {
                        write_record(
                            &mut ctx.output_stream,
                            path.to_string_lossy().as_ref(),
                            Some(link_target.to_string_lossy().as_ref()),
                            "dir",
                            None,
                            modified,
                        )?;
                    }
                    return enum_files_recursive(ctx, parent, depth + 1);
                }
            }
            Err(e) => {
                eprintln!(
                    "failed to get metadata of link target destination file({}): {:?}",
                    link_path.to_string_lossy(),
                    e
                );
            }
        }
    }
    if !ctx
        .include
        .iter()
        .any(|x| x.matches_with(path.to_string_lossy().as_ref(), ctx.match_option.clone()))
    {
        return Ok((ctx.with_path(parent), 0));
    }
    // let modified = meta
    //     .map(|meta| match meta.modified() {
    //         Ok(v) => Some(v),
    //         Err(e) => {
    //             eprintln!(
    //                 "get modified time failed({}): {:?}",
    //                 path.to_string_lossy(),
    //                 e
    //             );
    //             None
    //         }
    //     })
    //     .unwrap_or(None);
    if check_include_exclude_path(path.as_path(), &ctx.include, &ctx.exclude, &ctx.match_option) {
        let link_target_relative_path = match path.parent() {
            Some(v) => v.join(link_target.clone()),
            None => link_target.clone()
        };
        let target_meta = match std::fs::metadata(&link_target_relative_path) {
            Ok(v) => Some(v),
            Err(e) => {
                eprintln!("failed to get target metadata({:?}): {:?}", link_target_relative_path, e);
                None
            }
        };
        return output_symlink_info(ctx, path.as_path(), link_target.to_string_lossy().as_ref(), meta.as_ref(), target_meta);
        // write_record(
        //     &mut ctx.output_stream,
        //     path.to_string_lossy().as_ref(),
        //     Some(link_target.to_string_lossy().as_ref()),
        //     "link",
        //     None,
        //     modified,
        // )?;
    }
    Ok((ctx.with_path(parent), 0))
}

fn enum_files_recursive<'a>(
    mut ctx: FindContext<'a>,
    parent: &std::path::Path,
    depth: i32
) -> Result<(FindContext<'a>, u64)> {
    if depth >= ctx.max_depth {
        return Ok((ctx.with_path(parent), 0));
    }
    let readiter = match std::fs::read_dir(&ctx.path) {
        Ok(v) => v,
        Err(e) => {
            eprintln!(
                "error in iterating {}, skipped: {:?}",
                ctx.path.as_path().to_str().unwrap(),
                e
            );
            return Ok((ctx, 0));
        }
    };
    let current_path = ctx.path.clone();
    let mut dir_total = 0;
    for dentry in readiter {
        let mut current_total = 0;
        let dentry = match dentry {
            Ok(v) => Some(v),
            Err(e) => {
                eprintln!(
                    "failed to get directory entry({}): {:?}",
                    ctx.path.to_str().unwrap(),
                    e
                );
                None
            }
        };
        if let Some(dentry) = dentry {
            let fpath = dentry.path();
            if ctx
                .exclude
                .iter()
                .any(|x| x.matches_with(fpath.to_string_lossy().as_ref(), ctx.match_option.clone()))
            {
                continue;
            }
            let file_type = match dentry.file_type() {
                Ok(v) => Some(v),
                Err(e) => {
                    eprintln!("failed to get file type, skipped({:?}): {:?}", fpath, e);
                    None
                }
            };
            if let Some(file_type) = file_type {
                if file_type.is_symlink() {
                    let meta = match std::fs::metadata(fpath.clone()) {
                        Ok(v) => Some(v),
                        Err(e) => {
                            eprintln!(
                                "failed to get metadata of symlink({}): {:?}",
                                fpath.to_string_lossy(),
                                e
                            );
                            None
                        }
                    };
                    ctx = ctx.with_path(fpath.as_path());
                    let retval = retrieve_symlink(ctx, current_path.as_path(), meta, depth + 1)?;
                    ctx = retval.0;
                    current_total += retval.1;
                } else if file_type.is_dir() {
                    let last_write = match dentry.metadata() {
                        Ok(v) => match v.modified() {
                            Ok(v) => Some(v),
                            Err(_) => None,
                        },
                        Err(_) => None,
                    };
                    let new_ctx = ctx.with_path(fpath.as_path());
                    let retval = enum_files_recursive(new_ctx, current_path.as_path(), depth + 1)?;
                    ctx = retval.0;
                    if !ctx.leaf_only
                        && check_include_exclude_path(fpath.as_path(), &ctx.include, &ctx.exclude, &ctx.match_option)
                    {
                        write_record(
                            &mut ctx.output_stream,
                            fpath.as_path().to_string_lossy().as_ref(),
                            None,
                            "dir",
                            Some(retval.1),
                            last_write,
                        )?;
                    }
                    current_total += retval.1;
                } else if file_type.is_file() {
                    let rtval = output_file_info_dentry(ctx, &dentry)?;
                    ctx = rtval.0;
                    current_total += rtval.1;
                }
            }
        }
        dir_total += current_total;
    }
    Ok((ctx.with_path(parent), dir_total))
}

fn output_header<T>(writer: &mut RecordWriter<T>) -> Result<()>
where
    T: std::io::Write,
{
    writer.output_header()?;
    Ok(())
}

fn enum_files(pattern: &FindOption) -> Result<()> {
    let mut record_writer = create_record_writer(
        pattern.output.as_ref().map(|x| x.as_str()),
        pattern.output_format.as_ref(),
    )?;
    let mut is_first = true;
    for base_path in pattern.basepath.iter() {
        let mut ctx = FindContext::from_options(pattern, &mut record_writer, base_path)?;

        let rootpath = ctx.path.clone();
        if is_first {
            output_header(&mut ctx.output_stream)?;
            is_first = false;
        }
        let (root_ctx, root_size) = enum_files_recursive(ctx, rootpath.as_path(), 0)?;
        if root_ctx.output_total {
            write_record(&mut record_writer, rootpath.as_path().to_string_lossy().as_ref(), None, "dir", Some(root_size), None)?;
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let fopt = FindOption::parse();
    enum_files(&fopt)
}
