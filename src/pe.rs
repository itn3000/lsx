use std::path::Path;
use anyhow::Result;
use pelite::FileMap;
use pelite::pe64::PeFile as PeFile64;
use pelite::pe64::Pe as Pe64;
use pelite::pe32::PeFile as PeFile32;
use pelite::pe32::Pe as Pe32;
use pelite::Error as PeError;

pub fn read_version_from_dll(p: &Path) -> Result<(Option<String>, Option<String>)> {
    let fmap = FileMap::open(p)?;
    match PeFile64::from_bytes(&fmap) {
        Ok(pe) => {
            let resources = pe.resources()?;
            return get_versions_from_resource(resources);
        },
        Err(e) => {
            if let PeError::PeMagic = e {
                let pe = PeFile32::from_bytes(&fmap)?;
                let resources = pe.resources()?;
                return get_versions_from_resource(resources);
            } else {
                return Err(anyhow::Error::from(e));
            }
        }
    };
}

fn get_versions_from_resource(res: pelite::resources::Resources) -> Result<(Option<String>, Option<String>)> {
    let verinfo = res.version_info()?;
    for lang in verinfo.translation() {
        let product = verinfo.value(lang.to_owned(), "ProductVersion");
        let file = verinfo.value(lang.to_owned(), "FileVersion");
        if product.is_some() || file.is_some() {
            return Ok((file.map(|x| x.trim().to_owned()), product.map(|x| x.trim().to_owned())));
        }
    }
    // if no version info
    return Ok((None, None));
}