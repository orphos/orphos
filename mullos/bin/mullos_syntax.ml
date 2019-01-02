(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)

type number_literal_type =
  | ZType
  | QType
  | IntType of int
  | FloatType of int

type exp =
  | Bool of bool
  | Number of Q.t * number_literal_type
  | Text of string
  | Identifier of string
  | Unit

