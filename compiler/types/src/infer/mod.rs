mod arena;
mod solve;
mod ty;

use error::TypeError;

pub type Result<T> = std::result::Result<T, TypeError>;
