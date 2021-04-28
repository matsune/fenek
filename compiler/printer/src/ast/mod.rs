mod json;

pub fn print_stmt(stmt: &ast::Stmt) -> serde_json::Result<()> {
    let stmt: json::Stmt = stmt.into();
    let json = serde_json::to_string(&stmt)?;
    println!("{}", json);
    Ok(())
}
