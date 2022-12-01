use std::path::Path;

use super::*;
use thiserror::*;
use tracing::*;

#[derive(Debug)]
pub struct CodeDb {
    sql: rusqlite::Connection,
}

#[derive(Error, Debug)]
pub enum CodeDbError {
    #[error(transparent)]
    SqliteError(rusqlite::Error),

    #[error("Could not find {symbol_kind} named {symbol_raw}. Did you call `warp lift` on this workspace yet?")]
    UnknownSymbol {
        symbol_raw: String,
        symbol_kind: String,
    },

    #[error("Could not find file named {file}. Did you call `warp lift` on this workspace yet?")]
    UnknownFile { file: String },
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
                UNIQUE (source_path, source_hash, file_path)
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

    /// Find the Label that corresponds to a particular file.
    ///
    /// If the file is defined multiple times, it will return the first local file. If there is
    /// no local file, it will return the first remote file.
    ///
    pub async fn find_label_for_file(&self, file: &str) -> Result<Label, CodeDbError> {
        let mut query = self
            .sql
            .prepare(
                r#" SELECT label FROM files
                WHERE file_path = ?1
            "#,
            )
            .map_err(CodeDbError::SqliteError)?;

        let mut rows = query
            .query_map(rusqlite::params![file], |row| {
                let label_json: String = row.get(0).unwrap();
                let label: Label = serde_json::from_str(&label_json).unwrap();
                Ok(label)
            })
            .map_err(CodeDbError::SqliteError)?;

        // NOTE(@ostera): the heuristic here is to prefer local symbol over remote ones, but if
        // there are multiple remote ones, pick the first one.
        //
        // However, we may iterate over all the remote symbol before we find the local one, so we
        // have to save the first one until the end.
        //
        let mut first_remote_label = None;
        while let Some(Ok(label)) = rows.next() {
            if label.is_file() {
                return Ok(label);
            }
            if first_remote_label.is_none() {
                first_remote_label = Some(label);
            }
            continue;
        }

        if let Some(label) = first_remote_label {
            return Ok(label);
        }

        Err(CodeDbError::UnknownFile {
            file: file.to_string(),
        })
    }

    /// Find the Label that corresponds to a particular symbol.
    ///
    /// If the symbol is defined multiple times, it will return the first local symbol. If there is
    /// no local symbol, it will return the first remote symbol.
    ///
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

        // NOTE(@ostera): the heuristic here is to prefer local symbol over remote ones, but if
        // there are multiple remote ones, pick the first one.
        //
        // However, we may iterate over all the remote symbol before we find the local one, so we
        // have to save the first one until the end.
        //
        let mut first_remote_label = None;
        while let Some(Ok(label)) = rows.next() {
            if label.is_file() {
                return Ok(label);
            }
            if first_remote_label.is_none() {
                first_remote_label = Some(label);
            }
            continue;
        }

        if let Some(label) = first_remote_label {
            return Ok(label);
        }

        Err(CodeDbError::UnknownSymbol {
            symbol_raw: symbol_raw.to_string(),
            symbol_kind: symbol_kind.to_string(),
        })
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
                r#" DELETE FROM symbols
                    WHERE symbol_raw = ?1
                      AND symbol_kind = ?2
                "#,
                (symbol_raw, symbol_kind),
            )
            .map_err(CodeDbError::SqliteError)?;

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
