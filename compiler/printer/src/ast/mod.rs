mod json;

pub use json::*;

pub fn print_ast_fun(fun: &ast::Fun) -> serde_json::Result<()> {
    let fun: json::Fun = fun.into();
    let json = serde_json::to_string(&fun)?;
    println!("{}", json);
    Ok(())
}
