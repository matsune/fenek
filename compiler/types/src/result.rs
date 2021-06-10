use error::TypeError;

pub type Result<T> = std::result::Result<T, TypeError>;
