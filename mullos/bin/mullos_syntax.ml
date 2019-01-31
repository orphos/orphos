(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)

type number_literal_type = ZType | QType | IntType of int | FloatType of int

type number = Q.t * number_literal_type

type bin_op =
  [ `Add
  | `Substract
  | `Multiply
  | `Division
  | `Xor
  | `Reminder
  | `BitwiseLeftShift
  | `BitwiseRightShift
  | `Less
  | `Greater
  | `Equal
  | `NotEqual
  | `BitwiseAnd
  | `And
  | `BitwiseOr
  | `Or
  | `AddAsign
  | `SubstractAsign
  | `Asign
  | `Combine
  | `Remove
  | `Cons ]

type unary_op = Positive | Negative | Not | Deref | Ref | Raise | Lazy

type pat_bin_op = [`Colon | `Comma | `At]

type exp =
  | Bool of bool
  | Number of number
  | Text of string
  | Identifier of string
  | Unit
  | Apply of exp * exp
  | BinOp of exp * bin_op * exp * (bin_op * exp) list
  | UnaryOp of unary_op * exp
  | Tuple of exp list
  | IfThenElse of exp * exp * exp option
  | Seq of exp list
  | Lambda of pat * exp
  | Let of pat * pat list * exp * exp
  | Label of string * exp
  | Match of exp * pat_clause list

and pat_clause =
  | MatchPat of pat * exp option * exp
  | MatchException of pat * exp option * exp
  | MatchEffect of pat * exp option * exp

and pat =
  | PIdent of string
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
  | PLabel of string * pat
  | POr of pat * pat

type ty_bin_op = TComma | TArrow | TApply

type ty =
  | TIdent of string list
  | TVar of string
  | TPointer of ty
  | TNumber of number
  | TText of string
  | TBool of bool
  | TLazy of ty
  | TLabel of string * ty
  | TEff of ty * eff
  | TBinOp of ty * ty_bin_op * ty * (ty_bin_op * ty) list

and eff = ETy of ty list | EWildcard

type typedef =
  | VariantDef of string * ty list * deriving option * typedef_body
  | ExtensibleVariantDef of string * ctor

and typedef_body = Variant of ctor list | ExtensibleVariant

and ctor = string * ty option

and deriving = ty list

and definition = ValDef of pat list * exp
