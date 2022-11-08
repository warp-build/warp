use std::path::Path;

use super::*;
use thiserror::*;
use tracing::*;

#[derive(Debug)]
pub struct CodeDb {
    sql: rusqlite::Connection,
    //db: rocksdb::DB,
}

#[derive(Error, Debug)]
pub enum CodeDbError {
    #[error(transparent)]
    RocksDbError(rocksdb::Error),

    #[error(transparent)]
    SqliteError(rusqlite::Error),
}

impl CodeDb {
    pub async fn new(workspace: &Workspace) -> Result<Self, CodeDbError> {
        let db_path = workspace
            .paths
            .global_codedb_root
            .join(&workspace.paths.workspace_name);

        tokio::fs::create_dir_all(&db_path).await.unwrap();
        let sql = rusqlite::Connection::open(db_path.join("warp.db"))
            .map_err(CodeDbError::SqliteError)?;

        sql.execute(
            r"
            CREATE TABLE IF NOT EXISTS analysis (
                source_path TEXT,
                source_hash TEXT,
                label TEXT,
                UNIQUE (source_path, source_hash)
            );
            ",
            (),
        )
        .map_err(CodeDbError::SqliteError)?;

        sql.execute(
            r"
            CREATE TABLE IF NOT EXISTS symbols (
                source_path TEXT,
                source_hash TEXT,
                symbol_raw  TEXT,
                symbol_kind TEXT,
                label TEXT,
                UNIQUE (source_hash, symbol_raw, symbol_kind)
            );
            ",
            (),
        )
        .map_err(CodeDbError::SqliteError)?;

        sql.execute(
            r"
            CREATE TABLE IF NOT EXISTS files (
                source_path TEXT,
                source_hash TEXT,
                file_path TEXT,
                label TEXT,
                UNIQUE (source_path, source_hash)
            );
        ",
            (),
        )
        .map_err(CodeDbError::SqliteError)?;

        // let db = rocksdb::DB::open_default(db_path.join("warp.rocksdb")) .map_err(CodeDbError::RocksDbError)?;
        Ok(Self { sql })
    }

    pub async fn save_file(
        &self,
        label: &Label,
        source_file: &Path,
        source_hash: &str,
        file_path: &str,
    ) -> Result<(), CodeDbError> {
        self.sql
            .execute(
                r#" INSERT OR IGNORE
                    INTO files (source_path, source_hash, file_path, label)
                    VALUES (?1, ?2, ?3, ?4)
                "#,
                (
                    source_file.to_string_lossy(),
                    source_hash,
                    file_path,
                    serde_json::to_string(label).unwrap(),
                ),
            )
            .map(|_row_count| ())
            .map_err(CodeDbError::SqliteError)
    }

    pub async fn find_label_for_symbol(
        &self,
        symbol_raw: &str,
        symbol_kind: &str,
    ) -> Result<Label, CodeDbError> {
        let mut query = self
            .sql
            .prepare(
                r#" SELECT label FROM symbols
                WHERE symbol_raw  = ?1
                  AND symbol_kind = ?2
            "#,
            )
            .map_err(CodeDbError::SqliteError)?;

        let mut rows = query
            .query_map(rusqlite::params![symbol_raw, symbol_kind], |row| {
                let label_json: String = row.get(0).unwrap();
                let label: Label = serde_json::from_str(&label_json).unwrap();
                Ok(label)
            })
            .map_err(CodeDbError::SqliteError)?;

        let label = rows.next().unwrap().unwrap();
        Ok(label)
    }

    pub async fn save_symbol(
        &self,
        label: &Label,
        source_file: &Path,
        source_hash: &str,
        symbol_raw: &str,
        symbol_kind: &str,
    ) -> Result<(), CodeDbError> {
        self.sql
            .execute(
                r#" INSERT OR IGNORE
                    INTO symbols (source_path, source_hash, symbol_raw, symbol_kind, label)
                    VALUES (?1, ?2, ?3, ?4, ?5)
                "#,
                (
                    source_file.to_string_lossy(),
                    source_hash,
                    symbol_raw,
                    symbol_kind,
                    serde_json::to_string(label).unwrap(),
                ),
            )
            .map(|_row_count| ())
            .map_err(CodeDbError::SqliteError)
    }
}
