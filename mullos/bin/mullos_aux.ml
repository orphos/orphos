(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
*)

let concat_list_option = function None -> [] | Some xs -> xs

type oid = int

let current_oid = ref 0

let new_oid () =
  let ret = !current_oid in
  current_oid := ret + 1 ;
  ret

let with_oid x = (new_oid (), x)

exception NotImplemented of string

let noimpl feat = raise (NotImplemented feat)

exception Panic of string

let bug msg = raise (Panic ("[BUG] " ^ msg))
