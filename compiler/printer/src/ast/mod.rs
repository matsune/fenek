mod json;

pub use json::*;

pub fn print(root: &ast::Module) -> serde_json::Result<()> {
    let root: json::Module = root.into();
    let json = serde_json::to_string(&root)?;
    println!("{}", json);
    Ok(())
}
