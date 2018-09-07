#![feature(decl_macro)]
#![recursion_limit = "128"]

extern crate gll;

use gll::grammar::grammar;
use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    let lua = grammar!{
        /*Tokens = { Shebang? __ tokens:Token* % __ __ };
        Token = {
            Name:Name |
            Punct:Punct |
            Numeral:Numeral |
            LiteralString:LiteralString
        };*/

        __ = { { Comment | WhiteSpace }* };
        WhiteSpace = { { " " | "\t" | "\n" | "\r" }+ { !" " !"\t" !"\n" !"\r" } };

        Comment = {
            "--" { !"\n" (..) }* "\n" |
            MultiLineComment
        };
        MultiLineComment = { "--[" LiteralStringRaw "]" };

        Shebang = { "#!" { !"\n" (..) }* "\n" };

        NameStart = { ('a'..='z') | ('A'..='Z') | "_" };
        NameCont = { NameStart | ('0'..='9') };
        NotName = { !('a'..='z') !('A'..='Z') !"_" !('0'..='9') };
        Name = { NameStart NameCont* NotName };

        /*Punct = {
            "." | "," | ":" | ";" | "+" | "*" | { "-" !"-" } | "/" | "=" | "^" | "%" | "#" |
            "{" | "}" | "[" | "]" | "(" | ")" | "&" | "|" | "<" | ">" | "~"
        };*/

        Numeral = {
            ('0'..='9')+ { "." ('0'..='9')* }? { { "e" | "E" } { "+" | "-" }? ('0'..='9')+ }? NotName |
            { "0x" | "0X" } { ('0'..='9') | ('a'..='f') | ('A'..='F') }+ { "." { ('0'..='9') | ('a'..='f') | ('A'..='F') }* }? { { "p" | "P" } { "+" | "-" }? ('0'..='9')+ }? NotName
        };

        LiteralString = {
            SQ:{ "'" contents:LiteralStringSQContents "'" } |
            DQ:{ "\"" contents:LiteralStringDQContents "\"" } |
            Raw:{ "[" raw:LiteralStringRaw "]" }
        };

        LiteralStringSQContents = { { { !"\\" !"'" !"\n" !"\r" (..) } | LiteralStringEscape }* };
        LiteralStringDQContents = { { { !"\\" !"\"" !"\n" !"\r" (..) } | LiteralStringEscape }* };
        LiteralStringEscape = {
            { "\\" !"z" (..) } |
            { "\\z" WhiteSpace }
        };
        LiteralStringRaw = {
            { "[" LiteralStringRawContents "]" } |
            { "=" LiteralStringRaw "=" }
        };
        LiteralStringRawContents = { (..)* };

        Chunk = { Shebang? __ block:Block __ };
        Block = { stats:Stat* % __ { __ ret_stat:RetStat }? };
        Stat = {
            Semi:";" |
            Assign:{ vars:VarList __ "=" __ exps:ExpList } |
            Call:FunctionCall |
            Label:Label |
            Break:{ "break" NotName } |
            Goto:{ "goto" NotName __ label:Name } |
            Do:{ "do" NotName __ block:Block __ "end" NotName } |
            While:{ "while" NotName __ cond:Exp __ "do" NotName __ block:Block __ "end" NotName } |
            Repeat:{ "repeat" NotName __ block:Block __ "until" NotName __ cond:Exp } |
            If:{
                "if" NotName __ cond:Exp __ "then" NotName __ block:Block
                else_ifs:ElseIf*
                { __ "else" NotName __ else_block:Block }?
                __ "end" NotName
            } |
            For:{ "for" NotName __ name:Name __ "=" __ exp1:Exp __ "," __ exp2:Exp { __ "," __ exp3:Exp }? __ "do" NotName __ block:Block __ "end" NotName } |
            ForEach:{ "for" NotName __ names:NameList __ "in" NotName __ exps:ExpList __ "do" NotName __ block:Block __ "end" NotName } |
            Function:{ "function" NotName __ name:FuncName __ body:FuncBody } |
            LocFunction: { "local function" NotName __ name:Name __ body:FuncBody } |
            Local:{ "local" NotName __ names:NameList { __ "=" __ exps:ExpList }? }
        };
        ElseIf = { __ "elseif" NotName __ cond:Exp __ "then" NotName __ block:Block };
        RetStat = { "return" NotName { __ exps:ExpList }? { __ ";" }? };
        Label = { "::" __ name:Name __ "::" };
        FuncName = { func_name:Name+ % { __ "." __ }  { __ ":" __ name:Name }? };
        VarList = { var:Var+ % { __ "," __ } };
        Var = {
            Name:Name |
            Index:{ exp:PrefixExp __ "[" __ index:Exp __ "]" } |
            Field:{ exp:PrefixExp __ "." __ name:Name }
        };
        NameList = { name:Name+ % { __ "," __ } };
        ExpList = { exp:Exp+ % { __ "," __ } };
        Exp = {
            Nil:{ "nil" NotName } |
            False:{ "false" NotName } |
            True:{ "true" NotName } |
            Num:Numeral |
            Lit:LiteralString|
            Ellipsis:"..." |
            FuncDef:FunctionDef |
            Prefix:PrefixExp |
            TableCons:TableConstructor |
            Binop:{ lhs:Exp __ binop:Binop __ rhs:Exp } |
            Unop:{ unop:Unop __ exp:Exp }
        };
        PrefixExp = {
            Var:Var |
            Call:FunctionCall |
            Exp:{ "(" __ exp:Exp __ ")" }
        };
        FunctionCall = {
            Regular:{ exp:PrefixExp __ args:Args } |
            Method:{ exp:PrefixExp __ ":" __ name:Name __ args:Args }
        };
        Args = {
            Params:{ "(" { __ exps:ExpList }? __ ")" } |
            TableCons:TableConstructor |
            Lit:LiteralString
        };
        FunctionDef = { "function" NotName __ body:FuncBody };
        FuncBody = { "(" { __ params:ParList }? __ ")" __ block:Block __ "end" NotName };
        ParList = {
            Named:NameList |
            Ellipsis:"..." |
            NamedAndEllipsis:{ names:NameList __ "," __ "..." }
        };
        TableConstructor = { "{" { __ fields:FieldList }? __ "}" };
        FieldList = { fields:Field+ % { __ FieldSep __ } { __ sep:FieldSep }? };
        Field = {
            Computed:{ "[" __ index:Exp __ "]" __ "=" __ exp:Exp } |
            Named:{ name:Name __ "=" __ exp:Exp } |
            Unnamed:Exp
        };
        FieldSep = { "," | ";" };
        Binop = {
            "+" | { "-" !"-" } | "*" | "/" | "//" | "^" | "%" | "&" | "~" | "|" | ">>" | "<<" | ".." |
            "<" | "<=" | ">" | ">=" | "==" | "~=" | { "and" NotName } | { "or" NotName }
        };
        Unop = { "-" | { "not" NotName } | "#" | "~" };
    };
    fs::write(&out_dir.join("parse.rs"), lua.generate_rust()).unwrap();
}
