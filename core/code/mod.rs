mod model;
mod source_hasher;
pub use source_hasher::*;

use crate::model::{ConcreteTarget, ExecutableSpec, Signature};
use crate::resolver::TargetRegistry;
use crate::sync::{Arc, Mutex};
use crate::testing::TestMatcherRegistry;
use crate::worker::TaskRegistry;
use crate::Config;
use seahash::SeaHasher;
use std::hash::{Hash, Hasher};
use thiserror::Error;
use tracing::instrument;

const CODE_DATABASE_NAME: &str = "code.db";

#[derive(Debug)]
pub struct CodeDatabase {
    config: Config,
    sql: Arc<Mutex<rusqlite::Connection>>,
    test_matcher_registry: Arc<TestMatcherRegistry>,
    target_registry: Arc<TargetRegistry>,
    task_registry: Arc<TaskRegistry>,
}

impl CodeDatabase {
    pub fn new(
        config: Config,
        test_matcher_registry: Arc<TestMatcherRegistry>,
        target_registry: Arc<TargetRegistry>,
        task_registry: Arc<TaskRegistry>,
    ) -> Result<Self, CodeDatabaseError> {
        let sql = rusqlite::Connection::open(config.warp_root().join(CODE_DATABASE_NAME))?;

        sql.execute(
            r"
            CREATE TABLE IF NOT EXISTS signatures (
                target TEXT,
                source_hash TEXT,
                signatures TEXT
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
        Ok(Self {
            config,
            sql,
            target_registry,
            task_registry,
            test_matcher_registry,
        })
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

    #[instrument(name = "CodeDatabase::get_signatures", skip(self))]
    pub fn get_signatures(
        &self,
        concrete_target: &ConcreteTarget,
        source_hash: &str,
    ) -> Result<Option<Vec<Signature>>, CodeDatabaseError> {
        if !self.config.enable_code_database() {
            return Ok(None);
        }

        let sql = self.sql.lock().unwrap();

        let mut query = sql.prepare(
            r#" SELECT signatures FROM signatures
                WHERE target = ?1 AND source_hash = ?2
            "#,
        )?;

        let mut rows = query.query_map(
            rusqlite::params![concrete_target.original_target().to_string(), source_hash],
            |row| {
                let model_json: String = row.get(0).unwrap();
                let models: Vec<model::SignatureModel> = serde_json::from_str(&model_json).unwrap();

                let mut sigs = vec![];
                for model in models {
                    sigs.push(model.to_signature(
                        &self.test_matcher_registry,
                        &self.target_registry,
                        &self.task_registry,
                    ));
                }

                Ok(sigs)
            },
        )?;

        if let Some(row) = rows.next() {
            return Ok(Some(row.unwrap()));
        }

        Ok(None)
    }

    #[instrument(name = "CodeDatabase::save_signatures", skip(self))]
    pub fn save_signatures(
        &self,
        concrete_target: &ConcreteTarget,
        source_hash: &str,
        sigs: &[Signature],
    ) -> Result<(), CodeDatabaseError> {
        if !self.config.enable_code_database() {
            return Ok(());
        }

        let mut sig_models = vec![];
        for sig in sigs {
            let model = model::SignatureModel::from_signature(
                sig,
                &self.test_matcher_registry,
                &self.target_registry,
            );
            sig_models.push(model);
        }

        let sql = self.sql.lock().unwrap();

        sql.execute(
            r#" INSERT
                    INTO signatures (target, source_hash, signatures)
                    VALUES (?1, ?2, ?3)
                "#,
            (
                concrete_target.original_target().to_string(),
                source_hash,
                serde_json::to_string(&sig_models).unwrap(),
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
