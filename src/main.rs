use lexer::Lexer;
use ast::Parser;

mod lexer;
mod ast;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let query = "INSERT INTO mytable (id, name) VALUES (1, 'heinrich')";
    let mut parser = Parser::new(Lexer::new(query));

    match parser.parse() {
        Ok(ast) => println!("{:?}", ast),
        Err(e) => eprintln!("{}", e),
    }

    Ok(())
}
