(* Copyright (C) 2018 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier:LGPL-3.0-or-later
 *)
open Mullos_parser
open Mullos_lexer

let _ = print_endline (Mullos_llvm.hello_llvm ())
