#[derive(Debug, Default, Clone)]
pub enum Symbol {
    #[default]
    All,
    Named {
        name: String,
    },
}
