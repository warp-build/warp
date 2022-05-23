use super::*;

pub type LifterName = String;

#[derive(Debug, Clone)]
pub struct Lifter {
  name: LifterName,
  mnemonic: String,
  toolchains: Vec<Label>,
  sourceTree: SourceTree
}

impl Lifter {

    pub fn lift() -> () {
        trace!("Lifting")
    }
}
