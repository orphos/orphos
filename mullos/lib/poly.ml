(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: Apache-2.0 WITH LLVM-exception
 *)
(* PIL, polymorphic intermidiate language *)

open Aux

type id = string

type ty =
  | PIntType of int
  | PFloatType of int
  | PTypeIdent of id
  | PProductType of ty list
  | PArrow of ty * ty
  | PForall of id list * ty

type exp =
  | PInt of int64 * int
  | PFloat of string * int
  | PText of string
  | PIdent of id * ty
  | PProduct of exp list
  | PApply of exp * exp
  | PSwitch of (int * exp) list
  | PSeq of exp list
  | PLambda of id * ty * exp
  | PLet of (id * ty * exp) list * exp
  | PRecordLiteral of exp option * (string * exp) list
  | PRecordRestrictionLiteral of exp * string
  | PRecordSelection of exp * string
  | PPolymorphicVariantConstruction of string * exp
  | PHandle of exp * (id * id * exp) list
  | PConstruct of id * exp option

module Emit = struct
  let int = 1
  let float = 2
  let text = 3
  let ident = 4
  let product = 5
  let apply = 6
  let switch = 7
  let seq = 8
  let lambda = 9
  let letrec = 10
  let emit_uint8 out = Buffer.add_uint8 out

  let emit_varint out =
    let open Int64 in
    let emit_large a0 bits v =
      Buffer.add_uint8 out a0;
      let buf = Bytes.create 8 in
      Bytes.set_int64_be buf 0 v;
      Buffer.add_subbytes out buf 0 bits in function
    | v when v <= 240L -> Buffer.add_uint8 out (to_int v)
    | v when v <= 2287L ->
        let v = to_int v in
        Buffer.add_uint8 out ((v - 240) / 256 + 241);
        Buffer.add_uint8 out ((v - 2288) mod 256)
    | v when v <= 16777215L -> emit_large 250 3 v
    | v when v <= 4294967295L -> emit_large 251 4 v
    | v when v <= 1099511627775L -> emit_large 252 5 v
    | v when v <= 281474976710655L -> emit_large 253 6 v
    | v when v <= 72057594037927935L -> emit_large 254 7 v
    | v -> emit_large 255 8 v

  let compile out = function
    | PInt (value, bits) -> emit_uint8 out int; emit_uint8 out bits; emit_varint out value
    | _ -> noimpl "Poly.Emit.compile"
end

exception TypeError of string

let error message = raise (TypeError message)

module Env = Map.Make(String)

type env = ty Env.t

let lookup id = Env.find id

let extend id ty = Env.add id ty

let rec type_of env = function
  | PInt (_, bits) -> PIntType bits
  | PFloat (_, bits) -> PFloatType bits
  | PText _ -> PTypeIdent "Orphos::Core::Text::t"
  | PIdent (id, ty) ->
    if ty <> lookup id env then error "type of id does not match";
    ty
  | PProduct exps -> PProductType (exps |> List.map (type_of env))
  | PApply (fn, arg) -> PArrow (type_of env fn, type_of env arg)
  | PSwitch clauses ->
      (match clauses with
      | [] -> error "empty switch"
      | (_, h) :: t ->
        let ty = type_of env h in
        t |> List.iter (function _, exp -> if type_of env exp <> ty then error "type of switch clauses do not match");
        ty)
  | PSeq exps -> exps |> List.map (type_of env) |> BatList.last
  | PLambda (_, ty, exp) -> PArrow (ty, type_of env exp)
  | PLet (bindings, body) ->
      bindings |> List.iter (function _, ty, exp -> if type_of env exp <> ty then error "type of let do not match");
      type_of env body
  | PHandle (value, clauses) ->
      type_of env value |> ignore;
      (match clauses with
      | [] -> error "empty handle clause"
      | (_, _, h) :: t ->
        let ty = type_of env h in
        t |> List.iter (function _, _, exp -> if type_of env exp <> ty then error "type of handle clauses do not match");
        ty)
  | PConstruct _ -> assert false
  | _ -> noimpl "Poly.type_of"

