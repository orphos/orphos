(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)

type number_literal_type =
  | ZType
  | QType
  | IntType of int
  | FloatType of int

type number = Q.t * number_literal_type

type bin_op =
  | Add
  | Substract
  | Multiply
  | Division
  | Xor
  | Reminder
  | BitwiseLeftShift
  | BitwiseRightShift
  | Less
  | Greater
  | Equal
  | NotEqual
  | BitwiseAnd
  | And
  | BitwiseOr
  | Or
  | AddAsign
  | SubstractAsign
  | Asign
  | Combine
  | Remove

type unary_op =
  | Positive
  | Negative
  | Not
  | Deref
  | Ref
  | Raise
  | Lazy

type exp =
  | Bool of bool
  | Number of number
  | Text of string
  | Identifier of string
  | Unit
  | Apply of exp * exp
  | BinOp of exp * bin_op * exp
  | UnaryOp of unary_op * exp
  | Tuple of exp list
  | IfThenElse of exp * exp * exp option
  | Seq of exp list
  | Lambda of pat * exp
  | Let of pat * pat list * exp * exp
and pat =
  | PIdent of string list
  | PUnit
  | PBind of string * pat
  | PCtor of string * pat
  | PTuple of pat list
  | PCons of pat * pat
  | PWildcard
  | PText of string
  | PNumber of number
  | PBool of bool
  | PLazy of pat

type ty =
  | TIdent of string list
  | TVar of string
  | TApply of ty * ty
  | TTuple of ty list
  | TPointer of ty
  | TNumber of number
  | TText of string
  | TBool of bool
  | TLambda of ty * ty
  | TLazy of ty
  | TEff of ty * eff
and eff =
  | ETy of ty
  | ECombine of eff * eff
  | EWildcard

