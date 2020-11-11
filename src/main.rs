use anyhow::Result;
use clap::Clap;
use std::io::Write;
use std::rc::Rc;

#[derive(Clap, Debug)]
#[clap(version = env!("CARGO_PKG_VERSION"), author = "itn3000")]
struct FindOption {
    #[clap(short, long, about = "base path", default_value = ".")]
    basepath: String,
    #[clap(short, long, about = "include glob pattern(default: '**/*')")]
    include: Vec<String>,
    #[clap(short, long, about = "exclude glob pattern(default: empty)")]
    exclude: Vec<String>,
    #[clap(short, long, about = "output file path(default: stdout)")]
    output: Option<String>,
    #[clap(long, about = "follow symlink(default: false)")]
    follow_symlink: bool,
    #[clap(short, long, about = "max depth(default: 100)", default_value = "100")]
    max_depth: String,
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

struct FindContext {
    path: std::path::PathBuf,
    include: Rc<Vec<glob::Pattern>>,
    exclude: Rc<Vec<glob::Pattern>>,
    match_options: Rc<glob::MatchOptions>,
    output_stream: csv::Writer<OutputStream>,
    follow_symlink: bool,
    max_depth: i32,
}

impl FindContext {
    pub fn from_options(opts: &FindOption) -> Result<Self> {
        let includes: Result<Vec<glob::Pattern>, glob::PatternError> = if opts.include.len() > 0 {
            opts.include.iter().map(|x| glob::Pattern::new(x)).collect()
        } else {
            let pattern = glob::Pattern::new("**/*")?;
            Ok(vec![pattern])
        };
        let excludes: Result<Vec<glob::Pattern>, glob::PatternError> =
            opts.exclude.iter().map(|x| glob::Pattern::new(x)).collect();
        let match_options = glob::MatchOptions::default();
        let p = std::path::PathBuf::from(opts.basepath.as_str());
        let output_stream = match opts.output.as_ref() {
            Some(v) => OutputStream::File(std::fs::File::create(v)?),
            None => OutputStream::Stdout(std::io::stdout()),
        };
        let max_depth = opts.max_depth.parse::<i32>()?;
        Ok(FindContext {
            include: Rc::new(includes?),
            exclude: Rc::new(excludes?),
            match_options: Rc::new(match_options),
            path: p,
            output_stream: csv::Writer::from_writer(output_stream),
            follow_symlink: opts.follow_symlink,
            max_depth: max_depth,
        })
    }
    pub fn with_path(mut self, new_path: &std::path::Path) -> Self {
        self.path = std::path::PathBuf::from(new_path);
        self
    }
}

fn output_file_info(
    mut ctx: FindContext,
    path: &std::path::Path,
    meta: Option<std::fs::Metadata>,
) -> Result<FindContext> {
    let parent = ctx.path.clone();
    if !ctx.include.iter().any(|x| x.matches(path.to_string_lossy().as_ref())) {
        return Ok(ctx);
    }
    let (len, modified) = if let Some(meta) = meta {
        (Some(meta.len()), Some(meta.modified()))
    } else {
        (None, None)
    };
    let ststr = if let Some(modified) = modified {
        match modified {
            Ok(v) => {
                let st = chrono::DateTime::<chrono::Local>::from(v);
                st.format("%Y-%m-%d %H:%M:%S").to_string()
            }
            Err(e) => {
                eprintln!(
                    "failed to transform modified to datetime({}): {:?}",
                    path.to_string_lossy(),
                    e
                );
                String::new()
            }
        }
    } else {
        String::new()
    };
    ctx.output_stream.write_record(&[
        path.to_string_lossy().as_ref(),
        "file",
        format!("{}", len.unwrap_or(0u64)).as_str(),
        ststr.as_str(),
    ])?;
    Ok(ctx.with_path(parent.as_path()))
}

fn output_file_info_dentry(
    ctx: FindContext,
    dentry: &std::fs::DirEntry,
) -> Result<FindContext> {
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
    writer: &mut csv::Writer<T>,
    path: &str,
    file_type: &str,
    length: u64,
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
    writer.write_record(&[
        path,
        file_type,
        format!("{}", length).as_str(),
        ststr.as_str(),
    ])?;
    Ok(())
}

fn retrieve_symlink(
    mut ctx: FindContext,
    parent: &std::path::Path,
    meta: Option<std::fs::Metadata>,
    depth: i32,
) -> Result<FindContext> {
    if depth >= ctx.max_depth {
        return Ok(ctx.with_path(parent));
    }
    let path = ctx.path.clone();
    if ctx.follow_symlink {
        let link_path = match std::fs::read_link(path.clone()) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("failed to readlink({}): {:?}", path.to_string_lossy(), e);
                return Ok(ctx.with_path(parent));
            }
        };
        let link_path = if link_path.as_path().is_relative() {
            path.join(link_path)
        } else {
            link_path
        };
        match std::fs::metadata(link_path.clone()) {
            Ok(v) => {
                let ftype = v.file_type();
                if ftype.is_symlink() {
                    ctx = ctx.with_path(link_path.as_path());
                    return retrieve_symlink(ctx, parent, Some(v), depth + 1);
                }
                if ftype.is_file() {
                    ctx = output_file_info(ctx, link_path.as_path(), Some(v))?;
                    return Ok(ctx.with_path(parent));
                } else if ftype.is_dir() {
                    ctx = ctx.with_path(link_path.as_path());
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
    if !ctx.include.iter().any(|x| x.matches(path.to_string_lossy().as_ref())) {
        return Ok(ctx.with_path(parent));
    }
    let modified = meta
        .map(|meta| match meta.modified() {
            Ok(v) => Some(v),
            Err(e) => {
                eprintln!(
                    "get modified time failed({}): {:?}",
                    path.to_string_lossy(),
                    e
                );
                None
            }
        })
        .unwrap_or(None);
    write_record(
        &mut ctx.output_stream,
        path.to_string_lossy().as_ref(),
        "link",
        0,
        modified,
    )?;
    Ok(ctx.with_path(parent))
}

fn enum_files_recursive(mut ctx: FindContext, parent: &std::path::Path, depth: i32) -> Result<FindContext> {
    if depth >= ctx.max_depth {
        return Ok(ctx.with_path(parent));
    }
    let readiter = match std::fs::read_dir(&ctx.path) {
        Ok(v) => v,
        Err(e) => {
            eprintln!(
                "error in iterating {}, skipped: {:?}",
                ctx.path.as_path().to_str().unwrap(),
                e
            );
            return Ok(ctx);
        }
    };
    let current_path = ctx.path.clone();
    for dentry in readiter {
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
                .any(|x| x.matches(fpath.to_string_lossy().as_ref()))
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
                    ctx = retrieve_symlink(ctx, current_path.as_path(), meta, depth + 1)?;
                } else if file_type.is_dir() {
                    let last_write = match dentry.metadata() {
                        Ok(v) => match v.modified() {
                            Ok(v) => Some(v),
                            Err(_) => None
                        },
                        Err(_) => None
                    };
                    write_record(&mut ctx.output_stream, fpath.as_path().to_string_lossy().as_ref(), "dir", 0, last_write)?;
                    let new_ctx = ctx.with_path(fpath.as_path());
                    ctx = enum_files_recursive(new_ctx, current_path.as_path(), depth + 1)?;
                } else if file_type.is_file() {
                    ctx = output_file_info_dentry(ctx, &dentry)?;
                }
            }
        }
    }
    Ok(ctx.with_path(parent))
}

fn output_header<T>(writer: &mut csv::Writer<T>) -> Result<()>
where
    T: std::io::Write,
{
    writer.write_record(&["path", "file_type", "length", "last_modified"])?;
    Ok(())
}

fn enum_files(pattern: &FindOption) -> Result<()> {
    let mut ctx = FindContext::from_options(pattern)?;
    let rootpath = ctx.path.clone();
    output_header(&mut ctx.output_stream)?;
    enum_files_recursive(ctx, rootpath.as_path(), 0)?;
    Ok(())
}

fn main() -> Result<()> {
    let fopt = FindOption::parse();
    enum_files(&fopt)
}
