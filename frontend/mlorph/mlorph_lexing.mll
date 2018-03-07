{
open Mlorph_parsing
}
rule lex = parse
  | ['_' 'a'-'z' 'A'-'Z']+ as id
  | '`' ([^'`']* as id) '`' {
    IDENTIFIER (id, Lexing.lexeme_start_p lexbuf)
  }
  | eof { EOF }
