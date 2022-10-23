use std::path::PathBuf;

use super::*;

#[derive(Clone, Debug)]
pub enum RemoteWorkspaceConfig {
    GithubWorkspace {
        username: String,
        repository: String,
        git_ref: String,
        url: url::Url,
        prefix: String,
    },

    UrlWorkspace {
        url: url::Url,
        sha1: String,
        prefix: String,
    },

    LocalWorkspace {
        path: String,
    },
}

impl RemoteWorkspaceConfig {
    pub fn is_local(&self) -> bool {
        matches!(self, RemoteWorkspaceConfig::LocalWorkspace { .. })
    }

    pub fn hash(&self) -> &str {
        match self {
            RemoteWorkspaceConfig::UrlWorkspace { sha1, .. } => sha1,
            RemoteWorkspaceConfig::GithubWorkspace { git_ref, .. } => git_ref,
            RemoteWorkspaceConfig::LocalWorkspace { .. } => todo!(),
        }
    }

    pub fn prefix(&self) -> &str {
        match self {
            RemoteWorkspaceConfig::UrlWorkspace { prefix, .. } => prefix,
            RemoteWorkspaceConfig::GithubWorkspace { prefix, .. } => prefix,
            RemoteWorkspaceConfig::LocalWorkspace { .. } => todo!(),
        }
    }

    pub fn url(&self) -> &url::Url {
        match self {
            RemoteWorkspaceConfig::UrlWorkspace { url, .. } => url,
            RemoteWorkspaceConfig::GithubWorkspace { url, .. } => url,
            RemoteWorkspaceConfig::LocalWorkspace { .. } => todo!(),
        }
    }

    pub fn is_github(&self) -> bool {
        matches!(&self, RemoteWorkspaceConfig::GithubWorkspace { .. })
    }

    pub fn is_git_commit_or_url(&self) -> bool {
        match &self {
            RemoteWorkspaceConfig::GithubWorkspace { git_ref, .. } => {
                let regex = regex::Regex::new("[a-fA-F0-9]{40}").unwrap();
                git_ref.len() == 40 && regex.is_match(git_ref)
            }
            RemoteWorkspaceConfig::UrlWorkspace { .. } => true,
            RemoteWorkspaceConfig::LocalWorkspace { .. } => false,
        }
    }
}

impl WorkspaceConfig for RemoteWorkspaceConfig {
    fn path(&self) -> PathBuf {
        match &self {
            RemoteWorkspaceConfig::GithubWorkspace { url, git_ref, .. } => {
                let scheme_and_host = PathBuf::from(url.scheme()).join(url.host_str().unwrap());
                let org_and_repo: String = url
                    .path_segments()
                    .unwrap()
                    .take(2)
                    .collect::<Vec<&str>>()
                    .join("/");
                // NOTE(@ostera): path should be https/github.com/<org>/<repo>/<hash>
                scheme_and_host.join(org_and_repo).join(git_ref)
            }
            RemoteWorkspaceConfig::UrlWorkspace { url, .. } => {
                let scheme_and_host = PathBuf::from(url.scheme()).join(url.host_str().unwrap());
                scheme_and_host
            }
            RemoteWorkspaceConfig::LocalWorkspace { path } => PathBuf::from(path),
        }
    }
}

impl From<RemoteWorkspaceConfig> for RemoteWorkspaceFile {
    fn from(t: RemoteWorkspaceConfig) -> Self {
        match t {
            RemoteWorkspaceConfig::GithubWorkspace {
                username,
                repository,
                git_ref,
                ..
            } => Self {
                github: Some(format!("{}/{}", username, repository)),
                git_ref: Some(git_ref),
                ..Self::default()
            },
            RemoteWorkspaceConfig::UrlWorkspace { url, sha1, prefix } => Self {
                archive_url: Some(url),
                archive_sha1: Some(sha1),
                archive_prefix: Some(prefix),
                ..Self::default()
            },
            RemoteWorkspaceConfig::LocalWorkspace { path } => Self {
                path: Some(path),
                ..Self::default()
            },
        }
    }
}

impl TryFrom<RemoteWorkspaceFile> for RemoteWorkspaceConfig {
    type Error = WorkspaceFileError;

    fn try_from(value: RemoteWorkspaceFile) -> Result<Self, Self::Error> {
        if value.path.is_some() {
            return Ok(Self::LocalWorkspace {
                path: value.path.unwrap(),
            });
        }

        if value.archive_url.is_some() && value.archive_sha1.is_some() {
            return Ok(Self::UrlWorkspace {
                url: value.archive_url.unwrap(),
                sha1: value.archive_sha1.unwrap(),
                prefix: value.archive_prefix.unwrap_or_default(),
            });
        }

        if value.github.is_some() && value.git_ref.is_some() {
            let parts: Vec<String> = value
                .github
                .as_ref()
                .unwrap()
                .split('/')
                .into_iter()
                .map(|s| s.to_string())
                .collect();
            if parts.len() != 2 {
                return Err(WorkspaceFileError::RemoteWorkspaceError(
                    RemoteWorkspaceFileError::MalformedGithubString(value.github.unwrap()),
                ));
            }

            let username = parts[0].clone();
            let repository = parts[1].clone();
            let git_ref = value.git_ref.unwrap();
            let url = url::Url::parse(&format!(
                "https://github.com/{}/{}/archive/{}.zip",
                username, repository, git_ref
            ))
            .unwrap();

            return Ok(Self::GithubWorkspace {
                prefix: format!("{}-{}", &repository, &git_ref),
                username,
                repository,
                git_ref,
                url,
            });
        }

        Err(WorkspaceFileError::RemoteWorkspaceError(
            RemoteWorkspaceFileError::BadConfig(value),
        ))
    }
}
