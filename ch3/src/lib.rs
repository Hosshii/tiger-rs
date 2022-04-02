use error::Error;

mod error;
mod lexer;
mod position;
mod token;

pub use lexer::Lexer;

pub type Result<T> = std::result::Result<T, Error>;
