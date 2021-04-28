mod json;

pub fn print_expr(expr: &ast::Expr) -> serde_json::Result<()> {
    let expr: json::Expr = expr.into();
    let json = serde_json::to_string(&expr)?;
    println!("{}", json);
    Ok(())
}
