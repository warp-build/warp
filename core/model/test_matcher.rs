#[derive(Debug)]
pub struct TestMatcher(Vec<String>);

impl From<Vec<String>> for TestMatcher {
    fn from(value: Vec<String>) -> Self {
        Self(value)
    }
}

impl From<&[String]> for TestMatcher {
    fn from(value: &[String]) -> Self {
        value.to_vec().into()
    }
}
