use crate::rules::RuleStore;
use crate::sync::*;
use anyhow::{bail, Error};
use deno_core::{
    resolve_import, ModuleLoader, ModuleSource, ModuleSourceFuture, ModuleSpecifier, ModuleType,
};
use futures::FutureExt;
use std::pin::Pin;
use tokio::fs;
use tracing::*;

pub struct NetModuleLoader {
    pub rule_store: Arc<RuleStore>,
}

/// NOTE(@ostera): this feature copied from `deno-simple-module-loader`:
///     https://github.com/andreubotella/deno-simple-module-loader
impl ModuleLoader for NetModuleLoader {
    #[tracing::instrument(name = "NetModuleLoader::resolve", skip(self))]
    fn resolve(
        &self,
        specifier: &str,
        referrer: &str,
        _kind: deno_core::ResolutionKind,
    ) -> Result<ModuleSpecifier, Error> {
        Ok(resolve_import(specifier, referrer)?)
    }

    #[tracing::instrument(
        name = "NetModuleLoader::load",
        skip(self, _maybe_referrer, _is_dyn_import)
    )]
    fn load(
        &self,
        module_specifier: &ModuleSpecifier,
        _maybe_referrer: Option<ModuleSpecifier>,
        _is_dyn_import: bool,
    ) -> Pin<Box<ModuleSourceFuture>> {
        let rule_store = self.rule_store.clone();
        let module_specifier = module_specifier.clone();
        async move {
            let scheme = module_specifier.scheme().to_string();
            let string_specifier = module_specifier.to_string();

            let bytes: Vec<u8> = match scheme.clone().as_str() {
                "http" | "https" => {
                    let path = rule_store.get(&string_specifier).await?;
                    fs::read(path).await?
                }
                "file" => {
                    let path = match module_specifier.to_file_path() {
                        Ok(path) => path,
                        Err(_) => bail!("Invalid file URL."),
                    };
                    fs::read(path).await?
                }
                schema => bail!("Invalid schema {}", schema),
            };

            // Strip BOM
            let code = if bytes.starts_with(&[0xEF, 0xBB, 0xBF]) {
                bytes.as_slice()[3..].to_vec()
            } else {
                bytes
            }
            .into_boxed_slice();

            let module = ModuleSource {
                code,
                module_type: ModuleType::JavaScript,
                module_url_specified: string_specifier.clone(),
                module_url_found: string_specifier.to_string(),
            };

            Ok(module)
        }
        .boxed_local()
    }
}
