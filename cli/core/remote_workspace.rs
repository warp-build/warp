use super::*;

#[derive(Clone, Debug)]
pub enum RemoteWorkspace {
    GithubWorkspace {
        username: String,
        repository: String,
        git_ref: String,
    },

    UrlWorkspace {
        url: url::Url,
        sha1: String,
    },
}

impl From<RemoteWorkspace> for RemoteWorkspaceFile {
    fn from(t: RemoteWorkspace) -> Self {
        match t {
            RemoteWorkspace::GithubWorkspace {
                username,
                repository,
                git_ref,
            } => Self {
                github: Some(format!("{}/{}", username, repository)),
                git_ref: Some(git_ref),
                ..Self::default()
            },
            RemoteWorkspace::UrlWorkspace { url, sha1 } => Self {
                url: Some(url),
                sha1: Some(sha1),
                ..Self::default()
            },
        }
    }
}

impl TryFrom<RemoteWorkspaceFile> for RemoteWorkspace {
    type Error = WorkspaceFileError;

    fn try_from(value: RemoteWorkspaceFile) -> Result<Self, Self::Error> {
        if value.url.is_some() && value.sha1.is_some() {
            return Ok(Self::UrlWorkspace {
                url: value.url.unwrap(),
                sha1: value.sha1.unwrap(),
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
            return Ok(Self::GithubWorkspace {
                username,
                repository,
                git_ref: value.git_ref.unwrap(),
            });
        }
        return Err(WorkspaceFileError::RemoteWorkspaceError(
            RemoteWorkspaceFileError::BadConfig(value),
        ));
    }
}
