(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki3478@res.ac>
 *
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
module Make (Data : Syntax.Data) : sig
  val new_reader : unit -> Sedlexing.lexbuf -> Parser.Make(Data).token list
end
