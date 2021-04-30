mod json;
mod ty;

pub fn print(root: &hir::Module) -> serde_json::Result<()> {
    let module: json::Module = root.into();
    let json = serde_json::to_string(&module)?;
    println!("{}", json);
    Ok(())
}
