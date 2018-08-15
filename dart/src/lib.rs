#![feature(decl_macro)]

pub mod parse {
    include!(concat!(env!("OUT_DIR"), "/parse.rs"));
}

pub fn with_tokens<'a, R>(
    input: &'a str,
    f: impl for<'i> FnOnce(parse::Handle<'_, 'a, 'i, [parse::Token<'_, 'a, 'i>]>) -> R,
) -> R {
    parse::Parser::with_str(input, |mut parser, range| {
        let tokens = parse::Tokens::parse(&mut parser, range).unwrap();
        assert_eq!(tokens.node.range, range);
        f(tokens.one().unwrap().tokens)
    })
}

#[test]
fn lex() {
    with_tokens(
        r#"   foo123.n=x+y/*dart /**/ parser*/ 1+1 "${- stuff}" "#,
        |tokens| {
            let parser = tokens.parser;

            let v = tokens.collect::<Result<Vec<_>, _>>().unwrap();

            assert_eq!(
                v.iter()
                    .map(|x| format!("{:?}: {:?}", parser.input(x.node.range), x))
                    .collect::<Vec<String>>(),
                [
                    r#""foo123": 3..9 => Token::Ident(3..9)"#,
                    r#"".": 9..10 => Token::Punct(9..10)"#,
                    r#""n": 10..11 => Token::Ident(10..11)"#,
                    r#""=": 11..12 => Token::Punct(11..12)"#,
                    r#""x": 12..13 => Token::Ident(12..13)"#,
                    r#""+": 13..14 => Token::Punct(13..14)"#,
                    r#""y": 14..15 => Token::Ident(14..15)"#,
                    r#""/*dart /**/ parser*/": 15..35 => Token::Comment(15..35)"#,
                    r#""1": 36..37 => Token::Int(36..37)"#,
                    r#""+": 37..38 => Token::Punct(37..38)"#,
                    r#""1": 38..39 => Token::Int(38..39)"#,
                    "\"\\\"${- stuff}\\\"\": 40..52 => Token::Str(40..52 => StrLit::DQ { \
                     prefix: 41..41, \
                     interp: 41..51 => [Ok(41..51 => StrInterpol { \
                     tt: 42..51 => TokenTreeBraced { \
                     tt: 43..50 => [\
                     Ok(43..44 => TokenTree::Token { \
                     token: 43..44 => Token::Punct(43..44) \
                     }), \
                     Ok(45..50 => TokenTree::Token { \
                     token: 45..50 => Token::Ident(45..50) \
                     })\
                     ] \
                     } \
                     })], \
                     suffix: 51..51 \
                     })",
                ]
            );
        },
    );
}
