(* Copyright (C) 2018 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Mullos_parser

type context = {
  mutable indentation_stack: int list;
}

let new_lexer () =
  let context = { indentation_stack = [] } in
  let rec lex lexbuf =
    match%sedlex lexbuf with
    | "!=" -> EXCLAMATION_EQ
    | "&&" -> BIG_AMPERSAND
    | "++" -> BIG_PLUS
    | "+=" -> PLUS_EQ
    | "," -> COMMA
    | "-" -> HYPHEN
    | "--" -> BIG_HYPHEN
    | "-=" -> HYPHEN_EQ
    | "->" -> HYPHEN_GREATER
    | ".." -> BIG_DOT
    | "::" -> BIG_COLON
    | ":=" -> COLON_EQ
    | "==" -> BIG_EQ
    | "=>" -> EQ_GREATER
    | "case" -> CASE
    | "catch" -> CATCH
    | "class" -> CLASS
    | "data" -> DATA
    | "def" -> DEF
    | "deriving" -> DERIVING
    | "else" -> ELSE
    | "external" -> EXTERNAL
    | "false" -> BOOL
    | "fn" -> FN
    | "goto" -> GOTO
    | "if" -> IF
    | "instance" -> INSTANCE
    | "internal" -> INTERNAL
    | "label" -> LABEL
    | "let" -> LET
    | "match" -> MATCH
    | "raise" -> RAISE
    | "then" -> THEN
    | "true" -> BOOL
    | "type" -> TYPE
    | "unsafe" -> UNSAFE
    | "where" -> WHERE
    | "||" -> BIG_VERTICAL
    | '!' -> EXCLAMATION
    | '#' -> NUMBERSIGN
    | '$' -> DOLLAR
    | '&' -> AMPERSAND
    | '(' -> LPAREN
    | ')' -> RPAREN
    | '*' -> ASTERISK
    | '+' -> PLUS
    | '.' -> DOT
    | '/' -> SOLIDUS
    | ':' -> COLON
    | ';' -> SEMI
    | '<' -> LESS
    | '=' -> EQ
    | '>' -> GREATER
    | '@' -> AT
    | '^' -> CIRCUMFLEX
    | '_' -> LOWLINE
    | '{' -> LCBRACKET
    | '|' -> VERTICAL
    | '}' -> RCBRACKET
    | Plus (('a' .. 'z') | '_') -> IDENTIFIER
    | ('A' .. 'Z'), Star ('a' .. 'z') -> TYPE_IDENTIFIER
    | '`', Plus (('a' .. 'z') | '_') -> TYPEVAR_IDENTIFIER
    | 0x03b1 .. 0x03c9 -> TYPEVAR_IDENTIFIER
    | '"', Sub (any, '"'), '"' -> TEXT
    | ('1' .. '9'), Star ('0' .. '9'), Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | ('1' .. '9'), Star ('0' .. '9'), 'u', ('1' .. '9'), Star ('0' .. '9') -> NUMBER
    | ('1' .. '9'), Star ('0' .. '9') -> NUMBER
    | "0x", Plus (('0' .. '9') | ('a' .. 'f')) , Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | "0b", Plus ('0' | '1') , Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | '0', Star ('0' .. '7') , Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | Plus ('0' .. '9'), '.', Plus ('0' .. '9'), Opt ('E', Opt ('+' | '-'), Plus ('0' .. '9')), Opt ('f', Plus ('0' .. '9')) -> NUMBER
    | '.', Plus ('0' .. '9'), Opt ('E', Opt ('+' | '-'), Plus ('0' .. '9')), Opt ('f', Plus ('0' .. '9')) -> NUMBER
    | Plus ('0' .. '9'), '.', Opt ('E', Opt ('+' | '-'), Plus ('0' .. '9')), Opt ('f', Plus ('0' .. '9')) -> NUMBER
    | _ -> failwith "" in
  lex

