use crate::model::{ConcreteTarget, ExecutableSpec, Signature};
use crate::sync::{Arc, Mutex};
use crate::Config;
use seahash::SeaHasher;
use sha2::{Digest, Sha256};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use thiserror::Error;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tracing::instrument;

const CODE_DATABASE_NAME: &str = "code.db";

pub struct SourceHasher;

impl SourceHasher {
    pub async fn hash(file: &Path) -> Result<String, SourceHasherError> {
        let mut f =
            fs::File::open(&file)
                .await
                .map_err(|err| SourceHasherError::CouldNotOpenSource {
                    path: file.into(),
                    err,
                })?;
        let mut buffer = Vec::with_capacity(2048);
        f.read_to_end(&mut buffer)
            .await
            .map_err(|err| SourceHasherError::CouldNotReadSource {
                path: file.into(),
                err,
            })?;

        let mut s = Sha256::new();
        s.update(&buffer);
        Ok(format!("{:x}", s.finalize()))
    }
}

#[derive(Error, Debug)]
pub enum SourceHasherError {
    #[error("Source hasher could not read source file at {path:?} due to {err:?}")]
    CouldNotReadSource { path: PathBuf, err: std::io::Error },

    #[error("Source hasher could not open source file at {path:?} due to {err:?}")]
    CouldNotOpenSource { path: PathBuf, err: std::io::Error },
}

#[derive(Debug)]
pub struct CodeDatabase {
    config: Config,
    sql: Arc<Mutex<rusqlite::Connection>>,
}

impl CodeDatabase {
    pub fn new(config: Config) -> Result<Self, CodeDatabaseError> {
        let sql = rusqlite::Connection::open(config.warp_root().join(CODE_DATABASE_NAME))?;

        sql.execute(
            r"
            CREATE TABLE IF NOT EXISTS signatures (
                target TEXT,
                source_hash TEXT,
                signature TEXT
            );
        ",
            (),
        )?;

        sql.execute(
            r"
            CREATE TABLE IF NOT EXISTS executable_specs (
                target TEXT,
                signature_hash TEXT,
                executable_spec TEXT
            );
        ",
            (),
        )?;

        let sql = Arc::new(Mutex::new(sql));
        Ok(Self { config, sql })
    }

    #[instrument(name = "CodeDatabase::get_executable_spec", skip(self))]
    pub fn get_executable_spec(
        &self,
        sig: &Signature,
    ) -> Result<Option<ExecutableSpec>, CodeDatabaseError> {
        if !self.config.enable_code_database() {
            return Ok(None);
        }

        let sql = self.sql.lock().unwrap();

        let mut query = sql.prepare(
            r#" SELECT executable_spec FROM executable_specs
                WHERE signature_hash = ?1
            "#,
        )?;

        let sig_hash = {
            let mut s = SeaHasher::default();
            sig.hash(&mut s);
            format!("{:x}", s.finish())
        };

        let mut rows = query.query_map(rusqlite::params![sig_hash], |row| {
            let spec_json: String = row.get(0).unwrap();
            let spec: ExecutableSpec = serde_json::from_str(&spec_json).unwrap();
            Ok(spec)
        })?;

        let result = rows.next().map(|row| row.unwrap());

        Ok(result)
    }

    #[instrument(name = "CodeDatabase::save_executable_spec", skip(self))]
    pub fn save_executable_spec(
        &self,
        sig: &Signature,
        spec: &ExecutableSpec,
    ) -> Result<(), CodeDatabaseError> {
        if !self.config.enable_code_database() {
            return Ok(());
        }

        let sql = self.sql.lock().unwrap();

        let sig_hash = {
            let mut s = SeaHasher::default();
            sig.hash(&mut s);
            format!("{:x}", s.finish())
        };

        sql.execute(
            r#" INSERT
                    INTO executable_specs (target, signature_hash, executable_spec)
                    VALUES (?1, ?2, ?3)
                "#,
            (
                sig.target().to_string(),
                sig_hash,
                serde_json::to_string(spec).unwrap(),
            ),
        )?;
        Ok(())
    }

    #[instrument(name = "CodeDatabase::get_signature", skip(self))]
    pub fn get_signature(
        &self,
        concrete_target: &ConcreteTarget,
        source_hash: &str,
    ) -> Result<Option<Signature>, CodeDatabaseError> {
        if !self.config.enable_code_database() {
            return Ok(None);
        }

        let sql = self.sql.lock().unwrap();

        let mut query = sql.prepare(
            r#" SELECT signature FROM signatures
                WHERE target = ?1 AND source_hash = ?2
            "#,
        )?;

        let mut rows = query.query_map(
            rusqlite::params![concrete_target.original_target().to_string(), source_hash],
            |row| {
                let sig_json: String = row.get(0).unwrap();
                let sig: Signature = serde_json::from_str(&sig_json).unwrap();
                Ok(sig)
            },
        )?;

        let result = rows.next().map(|row| row.unwrap());

        Ok(result)
    }

    #[instrument(name = "CodeDatabase::save_signature", skip(self))]
    pub fn save_signature(
        &self,
        concrete_target: &ConcreteTarget,
        source_hash: &str,
        sig: &Signature,
    ) -> Result<(), CodeDatabaseError> {
        if !self.config.enable_code_database() {
            return Ok(());
        }

        let sql = self.sql.lock().unwrap();
        sql.execute(
            r#" INSERT
                    INTO signatures (target, source_hash, signature)
                    VALUES (?1, ?2, ?3)
                "#,
            (
                concrete_target.original_target().to_string(),
                source_hash,
                serde_json::to_string(sig).unwrap(),
            ),
        )?;
        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum CodeDatabaseError {
    #[error(transparent)]
    SqliteError(rusqlite::Error),
}

impl From<rusqlite::Error> for CodeDatabaseError {
    fn from(value: rusqlite::Error) -> Self {
        CodeDatabaseError::SqliteError(value)
    }
}
