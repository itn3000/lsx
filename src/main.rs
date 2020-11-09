use anyhow::Result;
use clap::Clap;

#[derive(Clap)]
#[clap(version = "0.1.0", author = "itn3000 <itn3000@gmail.com>")]
struct FindOption {
    #[clap(short, long)]
    basepath: String,
    #[clap(short, long)]
    include: Vec<String>,
    #[clap(short, long)]
    exclude: Vec<String>,
}

struct FileInfo {
    path: std::path::PathBuf,
    meta: std::fs::Metadata,
}

struct DirectoryIterator {
    rootpath: std::path::PathBuf,
    include: Vec<glob::Pattern>,
    exclude: Vec<glob::Pattern>,
    match_options: glob::MatchOptions,
}

impl DirectoryIterator {
    pub fn new(fopt: &FindOption) -> Result<Self> {
        let mut opts = glob::MatchOptions::new();
        opts.case_sensitive = false;
        opts.require_literal_leading_dot = true;
        opts.require_literal_separator = false;
        // let matches = glob::glob_with(pattern, opts)?;
        let includes: Result<Vec<glob::Pattern>, glob::PatternError> = fopt.include.iter().map(|x| glob::Pattern::new(x)).collect();
        let excludes: Result<Vec<glob::Pattern>, glob::PatternError> = fopt.exclude.iter().map(|x| glob::Pattern::new(x)).collect();
        let basepath = std::path::PathBuf::from(fopt.basepath);
        Ok(DirectoryIterator {
            include: includes?,
            exclude: excludes?,
            match_options: opts,
            rootpath: std::path::PathBuf::from(fopt.basepath),
        })
    }
    pub fn create_child(self, child_name: String) -> DirectoryIterator  {
        let p = std::path::PathBuf::from(self.rootpath);
        p.push(child_name);
        DirectoryIterator {
            exclude: self.exclude.clone(),
            include: self.include.clone(),
            match_options: self.match_options.to_owned(),
            rootpath: p
        }

    }
    fn path_to_fileinfo(p: &std::path::Path) -> Result<FileInfo> {
        let meta = std::fs::metadata(p)?;
        Ok(FileInfo {
            path: p.to_path_buf(),
            meta: meta,
        })
    }
}

impl Iterator for DirectoryIterator {
    type Item = Result<FileInfo>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(v) = self.matches.next() {
            match v {
                Ok(v) => return match Self::path_to_fileinfo(&v) {
                    Ok(v) => Some(Ok(v)),
                    Err(e) => Some(Err(e))
                },
                Err(e) => return Some(Err(anyhow::Error::from(e)))
            }
        } else {
            None
        }
    }
}

fn enum_files(pattern: &FindOption) -> Result<DirectoryIterator> {
    DirectoryIterator::new(pattern)
}

fn main() -> Result<()> {
    let fopt = FindOption::parse();
    for fi in enum_files(&fopt)? {
        let fi = fi?;
        println!("{},{:?},{},{},{},{:?},{:?}", fi.path.to_str().unwrap(), 
            fi.meta.file_type(), 
            fi.meta.is_dir(), 
            fi.meta.is_file(), 
            fi.meta.len(),
            fi.meta.permissions(),
            fi.meta.modified()?);
    }
    Ok(())
}
