pub use serde;

pub fn print_ast(root: &ast::Module) -> serde_json::Result<()> {
    let json = serde_json::to_string(&root)?;
    println!("{}", json);
    Ok(())
}
