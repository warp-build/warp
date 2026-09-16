#[derive(Default, Debug, Clone)]
pub enum TestMatcher {
    #[default]
    All,
    Patterns(Vec<String>),
}

impl TestMatcher {
    pub fn is_all(&self) -> bool {
        matches!(&self, Self::All)
    }

    pub fn raw(&self) -> &[String] {
        match self {
            TestMatcher::All => &[],
            TestMatcher::Patterns(pat) => pat,
        }
    }
}

impl From<Vec<String>> for TestMatcher {
    fn from(value: Vec<String>) -> Self {
        if value.is_empty() {
            Self::All
        } else {
            Self::Patterns(value)
        }
    }
}

impl From<&[String]> for TestMatcher {
    fn from(value: &[String]) -> Self {
        value.to_vec().into()
    }
}
