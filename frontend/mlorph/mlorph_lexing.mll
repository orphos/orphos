{
open Mlorph_parsing
}
rule lex = parse
  | eof { EOF }
