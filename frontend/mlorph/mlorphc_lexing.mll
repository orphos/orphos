{
open Mlorphc_parsing
}
rule lex = parse
  | eof { EOF }
