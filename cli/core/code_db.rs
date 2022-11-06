use std::path::{Path, PathBuf};

use super::*;
use thiserror::*;
use tracing::*;

pub struct CodeDb {
    sql: rusqlite::Connection,
    db: rocksdb::DB,
}

#[derive(Error, Debug)]
pub enum CodeDbError {
    #[error(transparent)]
    RocksDbError(rocksdb::Error),

    #[error(transparent)]
    SqliteError(rusqlite::Error),
}

impl CodeDb {
    pub fn new(workspace: &Workspace) -> Result<Self, CodeDbError> {
        let db_path = workspace.paths.warp_root.join("codedb");
        let db = rocksdb::DB::open_default(db_path.join("warp.rocksdb"))
            .map_err(CodeDbError::RocksDbError)?;

        let sql = rusqlite::Connection::open(db_path.join("warp.db"))
            .map_err(CodeDbError::SqliteError)?;

        sql.execute(
            r"
            CREATE TABLE IF NOT EXISTS source_analysis (
                source_file  TEXT UNIQUE,
                source_hash  TEXT UNIQUE
            );
        ",
            (),
        )
        .map_err(CodeDbError::SqliteError)?;

        Ok(Self { db, sql })
    }

    pub fn save_analysis(&self, source_file: &Path, source_hash: &str) -> Result<(), CodeDbError> {
        self.sql
            .execute(
                "INSERT INTO source_analysis (source_file, source_hash) VALUES (?1, ?2)",
                (source_file.to_string_lossy(), source_hash),
            )
            .map(|_row_count| ())
            .map_err(CodeDbError::SqliteError)
    }

    pub fn write_hello(&self) -> Result<(), CodeDbError> {
        self.db
            .put("hello-world", "we are live!")
            .map_err(CodeDbError::RocksDbError)
    }

    pub fn has_key<K>(&self, key: K) -> bool
    where
        K: AsRef<[u8]>,
    {
        self.db.key_may_exist(key)
    }
}
