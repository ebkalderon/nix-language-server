use codespan::Files;
use nix_parser2::lexer;

macro_rules! assert_tokens_match {
    ($expression_file_name:ident) => {
        #[test]
        fn $expression_file_name() {
            let mut files = Files::new();
            let file_id = files.add(
                stringify!($expression_file_name),
                include_str!(concat!(
                    env!("CARGO_MANIFEST_DIR"),
                    "/tests/lexer/",
                    stringify!($expression_file_name),
                    ".nix"
                )),
            );

            let source = files.source(file_id);
            let tokens: Vec<_> = lexer::tokenize(source)
                .inspect(|t| {
                    println!(
                        "Kind: {:?}, Span: {:?}",
                        t.kind,
                        files.source_slice(file_id, t.span)
                    )
                })
                .collect();

            let expected: Vec<_> = serde_json::from_str(include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/tests/lexer/",
                stringify!($expression_file_name),
                ".json"
            )))
            .expect("JSON parsing failed");

            assert_eq!(tokens, expected);
        }
    };

    (update $expression_file_name:ident) => {
        #[test]
        fn $expression_file_name() {
            let mut files = Files::new();
            let file_id = files.add(
                stringify!($expression_file_name),
                include_str!(concat!(
                    env!("CARGO_MANIFEST_DIR"),
                    "/tests/lexer/",
                    stringify!($expression_file_name),
                    ".nix"
                )),
            );

            let source = files.source(file_id);
            let tokens: Vec<_> = lexer::tokenize(source)
                .inspect(|t| {
                    println!(
                        "Kind: {:?}, Span: {:?}",
                        t.kind,
                        files.source_slice(file_id, t.span)
                    )
                })
                .collect();

            let json = serde_json::to_string_pretty(&tokens).unwrap();
            std::fs::write(
                concat!(
                    env!("CARGO_MANIFEST_DIR"),
                    "/tests/lexer/",
                    stringify!($expression_file_name),
                    ".json"
                ),
                json,
            )
            .unwrap();
        }
    };
}

assert_tokens_match!(trivia);
assert_tokens_match!(literals);
assert_tokens_match!(punctuation);
assert_tokens_match!(operators);
