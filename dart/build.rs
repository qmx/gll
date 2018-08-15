#![feature(decl_macro)]
#![recursion_limit = "128"]

extern crate gll;

use gll::grammar::grammar;
use std::env;
use std::fs::File;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    let mut dart = grammar!{
        Tokens = { WhiteSpace tokens:Token* % WhiteSpace WhiteSpace };
        Token = {
            Comment:Comment |
            Ident:Ident |
            Punct:Punct |
            Int:IntLit |
            Str:StrLit
        };

        WhiteSpace = { { " " | "\t" | "\n" | "\r" }* { !" " !"\t" !"\n" !"\r" } };

        IdentStart = { ('a'..='z') | ('A'..='Z') | "_" | "$" };
        IdentCont = { IdentStart | ('0'..='9') };
        NotIdent = { !('a'..='z') !('A'..='Z') !"_" !"$" !('0'..='9') };
        Ident = { IdentStart IdentCont* NotIdent };

        Comment = {
            "//" { !"\n" (..) }* "\n" |
            MultiLineComment
        };
        MultiLineComment = { "/*" { !"/*" !"*/" (..) | MultiLineComment }* "*/" };

        Punct = {
            "." | "," | ":" | ";" | "+" | "*" | "-" | { "/" !"/" !"*" } | "=" | "^" | "%" | "#" |
            "{" | "}" | "[" | "]" | "(" | ")" | "&" | "|" | "!" | "<" | ">" | "@" | "~" | "?"
        };

        IntLit = { ('0'..='9') IdentCont* NotIdent };

        StrLit = {
            SQ:{ "'" prefix:StrLitSQContents interp:StrInterpol* % StrLitSQContents suffix:StrLitSQContents "'" } |
            DQ:{ "\"" prefix:StrLitDQContents interp:StrInterpol* % StrLitDQContents suffix:StrLitDQContents "\"" } |
            TSQ:{ "'''" prefix:StrLitTSQContents interp:StrInterpol* % StrLitTSQContents suffix:StrLitTSQContents "'''" } |
            TDQ:{ "\"\"\"" prefix:StrLitTDQContents interp:StrInterpol* % StrLitTDQContents suffix:StrLitTDQContents "\"\"\"" } |
            RawSQ:{ "r'" contents:StrLitRawSQContents "'" } |
            RawDQ:{ "r\"" contents:StrLitRawDQContents "\"" } |
            RawTSQ:{ "r'''" contents:StrLitRawTSQContents "'''" } |
            RawTDQ:{ "r\"\"\"" contents:StrLitRawTDQContents "\"\"\"" }
        };

        StrLitSQContents = { { { !"\\" !"'" !"$" !"\n" !"\r" (..) } | { "\\" !"\n" !"\r" (..) } }* };
        StrLitDQContents = { { { !"\\" !"\"" !"$" !"\n" !"\r" (..) } | { "\\" !"\n" !"\r" (..) } }* };
        StrLitTSQContents = { { { !"\\" !"'''" !"$" (..) } | { "\\" (..) } }* };
        StrLitTDQContents = { { { !"\\" !"\"\"\"" !"$" (..) } | { "\\" (..) } }* };
        StrLitRawSQContents = { { !"'" !"\n" !"\r" (..) }* };
        StrLitRawDQContents = { { !"\"" !"\n" !"\r" (..) }* };
        StrLitRawTSQContents = { { !"'''" (..) }* };
        StrLitRawTDQContents = { { !"\"\"\"" (..) }* };

        StrInterpol = { "$" { IdentNoDollar | tt:TokenTreeBraced } };
        IdentNoDollarStart = { ('a'..='z') | ('A'..='Z') | "_" };
        IdentNoDollarCont = { IdentNoDollarStart | ('0'..='9') };
        IdentNoDollar = { IdentNoDollarStart IdentNoDollarCont* NotIdent };
        TokenTreeBraced = { "{" WhiteSpace tt:TokenTree* % WhiteSpace WhiteSpace "}" };
        TokenTree = {
            Token:{ !"{" !"}" token:Token } |
            Braced:TokenTreeBraced
        };
    };
    dart.generate(&mut File::create(&out_dir.join("parse.rs")).unwrap());
}
