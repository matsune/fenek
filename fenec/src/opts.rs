use clap::Clap;
use std::str::FromStr;

#[derive(Clap)]
#[clap(version = "1.0", author = "matsune <yuma.matsune@gmail.com>")]
pub struct Opts {
    #[clap(
        short, long, use_delimiter = true, multiple = true,
        possible_values = &["ast", "llvm-ir", "llvm-bc", "asm", "obj", "link"],
        default_value = "link",
        about = "types of output for the compiler to emit"
    )]
    pub emit: Vec<Emit>,
    #[clap(required = true, about = "source file to compile")]
    pub src: String,
}

#[derive(Debug)]
pub enum Emit {
    Ast,
    LlvmIr,
    LlvmBc,
    Asm,
    Obj,
    Link,
}

impl FromStr for Emit {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "ast" => Ok(Emit::Ast),
            "llvm-ir" => Ok(Emit::LlvmIr),
            "llvm-bc" => Ok(Emit::LlvmBc),
            "asm" => Ok(Emit::Asm),
            "obj" => Ok(Emit::Obj),
            "link" => Ok(Emit::Link),
            _ => Err("no match"),
        }
    }
}
