(* Copyright (C) 2018 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Llvm

let hello_llvm () = begin
  let c = create_context() in
  try
    let m = create_module c "hello module" in
    try
      let i32_type = i32_type c in
      let f_type = function_type i32_type [||] in
      let f = define_function "get_answer" f_type m in
      let builder = builder_at_end c (entry_block f) in
      let _ = build_ret (const_int i32_type 42) builder in
      string_of_llmodule m
    with e ->
      dispose_module m;
      raise e
  with e ->
    dispose_context c;
    raise e
end
