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
  | Apply of exp * exp
  | Tuple of exp list
  | IfThenElse of exp * exp * exp option
  | Seq of exp * exp
  | Lazy of exp

type pat =
  | PIdent of string list
  | PUnit
  | PBind of string * pat
  | PCtor of string * pat
  | PTuple of pat list
  | PCons of pat * pat
  | PWildcard
  | PText of string
  | PNumber of Q.t * number_literal_type
  | PBool of bool
  | PLazy of pat

type ty =
  | TIdent of string list
  | TVar of string
  | TApply of ty * ty list
  | TTuple of ty list
  | TPointer of ty
  | TNumber of Q.t * number_literal_type
  | TText of string
  | TBool of bool
  | TLambda of ty * ty
  | TLazy of ty
  | TEff of ty * eff
and eff =
  | ETy of ty
  | ECombine of eff * eff
  | EWildcard

