/// The `FsResolver` knows how to resolve a particular `Target` by looking into the file system and
/// determining if this is in fact a file on disk, and sending the sources to a `Tricorder` for
/// analysis.
#[derive(Clone, Debug)]
pub struct FsResolver;

impl FsResolver {
    pub fn new() -> Self {
        Self
    }
}
