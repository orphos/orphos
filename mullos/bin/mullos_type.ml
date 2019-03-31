(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki3478@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
type level = int

type ty =
  [ `TLongId of Mullos_syntax.long_id
  | `TApply of ty list * ty
  | `TArrow of ty * ty
  | `TTuple of ty list
  | `TLazy of ty
  | `TVar of tvar ref ]

and tvar = Unbound of int * level | Link of ty | Generic of int
