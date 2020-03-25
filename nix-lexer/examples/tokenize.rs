use std::io::Read;

use codespan::Files;

fn main() {
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer).unwrap();

    let mut files = Files::new();
    let file_id = files.add("<stdin>", &buffer);
    let source = files.source(file_id);

    for token in nix_lexer::tokenize(&source) {
        println!("{}", token.display(&source));
    }
}
