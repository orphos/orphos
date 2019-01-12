(* Copyright (C) 2018 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Base
open Llvm

let hello_llvm () =
  let c = create_context () in
  Exn.protect
    ~f:(fun () ->
      let m = create_module c "hello module" in
      Exn.protect
        ~f:(fun () ->
          let i32_type = i32_type c in
          let f_type = function_type i32_type [||] in
          let f = define_function "get_answer" f_type m in
          let builder = builder_at_end c (entry_block f) in
          let _ = build_ret (const_int i32_type 42) builder in
          string_of_llmodule m )
        ~finally:(fun () -> dispose_module m) )
    ~finally:(fun () -> dispose_context c)
