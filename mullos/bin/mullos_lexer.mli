(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
module Make (Data : Mullos_syntax.Data) : sig
  val new_reader :
    unit -> Sedlexing.lexbuf -> Mullos_parser.Make(Data).token list
end
