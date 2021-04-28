mod json;
mod ty;

pub fn print_hir_fun(fun: &hir::Fun) -> serde_json::Result<()> {
    let fun: json::Fun = fun.into();
    let json = serde_json::to_string(&fun)?;
    println!("{}", json);
    Ok(())
}
