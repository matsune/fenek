use error::TypeError;

pub mod arena;
pub mod solve;
pub mod ty;

pub type Result<T> = std::result::Result<T, TypeError>;
