use codespan::Files;

macro_rules! assert_tokens_match {
    ($expression_file_name:ident) => {
        #[test]
        fn $expression_file_name() {
            let mut files = Files::new();
            let file_id = files.add(
                stringify!($expression_file_name),
                include_str!(concat!(
                    env!("CARGO_MANIFEST_DIR"),
                    "/tests/",
                    stringify!($expression_file_name),
                    ".nix"
                )),
            );

            let source = files.source(file_id);
            let actual = nix_lexer::tokenize(&source).map(|t| t.display(&source).to_string());

            let expected = include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/tests/",
                stringify!($expression_file_name),
                ".snap"
            ))
            .split_terminator('\n');

            for (actual, expect) in actual.zip(expected) {
                assert_eq!(actual, expect);
            }
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
            let tokens: Vec<_> = lexer::tokenize(&source)
                .map(|t| t.display(&source).to_string())
                .collect();

            std::fs::write(
                concat!(
                    env!("CARGO_MANIFEST_DIR"),
                    "/tests/lexer/",
                    stringify!($expression_file_name),
                    ".snap"
                ),
                tokens.join("\n"),
            )
            .unwrap();
        }
    };
}

assert_tokens_match!(trivia);
assert_tokens_match!(literals);
assert_tokens_match!(punctuation);
assert_tokens_match!(operators);
