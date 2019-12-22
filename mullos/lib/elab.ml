(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki3478@res.ac>
 *
 * SPDX-Identifier: Apache-2.0 WITH LLVM-exception
 *)
open Aux
open Syntax

type data = {mutable ty_field: Syntax.Type.ty option; oid: oid}

let get_ty = function
  | {ty_field= Some ty; _} -> ty
  | {ty_field= None; _} -> bug "ty is not allocated yet"

let set_ty data ty = data.ty_field <- Some ty

let get_ty_of = function data, _ -> get_ty data

let set_ty_of = function data, _ -> fun ty -> data.ty_field <- Some ty

module ElabData = struct
  type t = data

  let allocate () = {ty_field= None; oid= new_oid ()}
end

module Tree = Syntax.Make (ElabData)
open Tree
open Syntax.Type

exception TypeError of string

let error msg = raise (TypeError ("type error: " ^ msg))

module LongIdIsOrdered = struct
  type t = long_id

  let compare left right =
    let rec aux = function
        | [], [] -> 0
        | h1 :: t1, h2 :: t2 ->
            let x = String.compare h1 h2 in
            if x == 0 then aux (t1, t2) else x
        | _ :: _, [] -> 1
        | [], _ :: _ -> -1 in
    let LongId left, LongId right = (left, right) in
    aux (left, right)
end

module IdMap = Map.Make (LongIdIsOrdered)

type value = Exp of exp | CtorValue of ctor | Captured of pat

let get_ty_of_value = function
  | Exp (data, _) | CtorValue (data, _) | Captured (data, _) -> get_ty data

type env = {values: value IdMap.t; types: type_decl IdMap.t}

let empty = {values= IdMap.empty; types= IdMap.empty}

let extend_value env name value =
  {env with values= IdMap.add name value env.values}

let extend_type env name type_exp =
  {env with types= IdMap.add name type_exp env.types}

let resolve_value env name = IdMap.find name env.values

let resolve_type env name = IdMap.find name env.types

let occurs_check_adjust_levels tvar_id tvar_level ty =
  let rec f = function
    | TVar {contents= Link ty} -> f ty
    | TVar {contents= Generic _} -> bug "occurs check to generic variable"
    | TVar ({contents= Unbound (other_id, other_level)} as other_tvar) ->
        if other_id = tvar_id then error "recursive types"
        else if other_level > tvar_level then
          other_tvar := Unbound (other_id, tvar_level)
        else ()
    | TApp (ty_args, ty) -> f ty ; List.iter f ty_args
    | TFun (param_ty, return_ty) -> f param_ty ; f return_ty
    | TLongId _ -> ()
    | TProduct tys -> List.iter f tys
    | TVariant (_, _, ctors) -> List.iter (function _, ty -> f ty) ctors
  in
  f ty

let rec unify ty1 ty2 =
  if ty1 == ty2 then ()
  else
    match (ty1, ty2) with
    | TLongId long_id1, TLongId long_id2 when long_id1 = long_id2 -> ()
    | TApp (ty_args1, ty1), TApp (ty_args2, ty2) ->
        unify ty1 ty2 ;
        List.iter2 unify ty_args1 ty_args2
    | TFun (param_ty1, return_ty1), TFun (param_ty2, return_ty2) ->
        unify param_ty1 param_ty2 ;
        unify return_ty1 return_ty2
    | TVar {contents= Link ty1}, ty2 | ty1, TVar {contents= Link ty2} ->
        unify ty1 ty2
    | TVar {contents= Unbound (id1, _)}, TVar {contents= Unbound (id2, _)}
      when id1 = id2 ->
        bug "multiple instance of a particular type variable."
    | TVar ({contents= Unbound (id, level)} as tvar), ty
     |ty, TVar ({contents= Unbound (id, level)} as tvar) ->
        occurs_check_adjust_levels id level ty ;
        tvar := Link ty
    | TProduct xs, TProduct ys -> List.iter2 unify xs ys
    | _, _ -> error "cannot unify types "

let rec generalize level = function
  | TVar {contents= Unbound (id, other_level)} when other_level > level ->
      TVar (ref (Generic id))
  | TApp (ty_args, ty) ->
      TApp (List.map (generalize level) ty_args, generalize level ty)
  | TFun (param_ty, return_ty) ->
      TFun (generalize level param_ty, generalize level return_ty)
  | TProduct tys -> TProduct (List.map (generalize level) tys)
  | TVariant (params, name, ctors) ->
      TVariant
        ( params
        , name
        , List.map (function name, ty -> (name, generalize level ty)) ctors )
  | TVar {contents= Link ty} -> generalize level ty
  | (TVar {contents= Generic _} | TVar {contents= Unbound _} | TLongId _) as ty
    ->
      ty

let instantiate level ty =
  let id_var_map = Hashtbl.create 10 in
  let rec f ty =
    match ty with
    | TLongId _ -> ty
    | TVar {contents= Link ty} -> f ty
    | TVar {contents= Generic id} -> (
      try Hashtbl.find id_var_map id with Not_found ->
        let var = new_var level in
        Hashtbl.add id_var_map id var ;
        var )
    | TVar {contents= Unbound _} -> ty
    | TApp (ty_args, ty) -> TApp (List.map f ty_args, f ty)
    | TFun (param_ty, return_ty) -> TFun (f param_ty, f return_ty)
    | TProduct tys -> TProduct (List.map f tys)
    | TVariant (params, name, ctors) ->
        TVariant
          (params, name, List.map (function name, ty -> (name, f ty)) ctors)
  in
  f ty

let rec elab_type env = function
  | data, type_exp ->
      let elab = elab_type env in
      let ty =
        match type_exp with
        | TIdent id -> resolve_type env id |> get_ty_of
        | TGeneric id -> resolve_type env (long_id [id]) |> get_ty_of
        | TLazy _ -> noimpl "lazy"
        | TLabel _ -> noimpl "record"
        | TEff _ -> noimpl "effect"
        | TRecord _ -> noimpl "record"
        | TPolymorphicVariant _ -> noimpl "polymorphic variant"
        | TOr _ -> noimpl "union type"
        | TRefinement _ -> noimpl "refinement type"
        | TGiven _ -> noimpl "given type"
        | TArrow (param, ret) -> TFun (elab param, elab ret)
        | TTuple elems -> TProduct (elems |> List.map elab)
        | TApply (params, applicant) ->
            TApp (params |> List.map elab, elab applicant)
      in
      set_ty data ty ; ty

let rec elab_pat (env : env) (level : level) = function
  | data, pat' ->
      let ty =
        match pat' with
        | PIdent _ -> new_var level
        | PUnit -> TLongId (LongId ["unit"])
        | PCapture (_, pat) -> elab_pat env level pat
        | PCtor _ -> noimpl "variant constructor pattern"
        | PTuple pats ->
            TProduct (List.map (fun pat -> elab_pat env level pat) pats)
        | PWildcard -> new_var level
        | PText _ -> TLongId (LongId ["text"])
        | PNumber _ -> TLongId (LongId ["i32"])
        | PBool _ -> TLongId (LongId ["bool"])
        | PLazy _ -> noimpl "lazy pattern"
        | POr (pat1, pat2) ->
            unify (elab_pat env level pat1) (elab_pat env level pat2) ;
            noimpl "union pattern"
        | PListLiteral _ -> noimpl "list pattern"
        | PArrayLiteral _ -> noimpl "array pattern"
        | PPolymorphicVariant _ -> noimpl "polymorphic variant pattern"
      in
      set_ty data ty ; ty

let rec elab_exp env level = function
  | data, exp' ->
      let elab = elab_exp env level in
      let ret =
        match exp' with
        | Ident name -> (
          try instantiate level (resolve_value env name |> get_ty_of_value)
          with Not_found -> error "variable not found" )
        | Lambda (((param_data, PIdent param_name) as param), body) ->
            let param_ty = new_var level in
            set_ty param_data param_ty ;
            let env =
              extend_value env (LongId [param_name]) (Captured param)
            in
            let return_ty = elab_exp env level body in
            TFun (param_ty, return_ty)
        | Lambda ((_, _), _) -> noimpl "pattern param"
        | Let (((_, PIdent name) as bindant), _, value, body) ->
            let value_ty = elab_exp env (level + 1) value in
            let generalized_ty = generalize level value_ty in
            set_ty data generalized_ty ;
            elab_exp
              (extend_value env (LongId [name]) (Captured bindant))
              level body
        | Let _ -> noimpl "binding to pattern"
        | LetRec (lets, body) ->
            let rec allocate_type_vars env = function
              | (((pat_data, PIdent name) as bindant), _, _) :: t ->
                  let ty = new_var (level + 1) in
                  set_ty pat_data ty ;
                  allocate_type_vars
                    (extend_value env (LongId [name]) (Captured bindant))
                    t
              | ((_, _), _, _) :: _ -> noimpl "binding to pattern"
              | [] -> env
            in
            let env = allocate_type_vars env lets in
            let rec elabBindees env = function
              | (((pat_data, PIdent name) as bindant), _, value) :: t ->
                  let value_ty = elab_exp env (level + 1) value in
                  set_ty pat_data value_ty ;
                  elabBindees
                    (extend_value env (LongId [name]) (Captured bindant))
                    t
              | [] -> env
              | ((_, _), _, _) :: _ -> noimpl "binding to pattern"
            in
            let env = elabBindees env lets in
            (* elab_exp body *)
            elab_exp env level body
        | Apply (fn, arg) -> (
          match elab_exp env level fn with
          | TFun (param, ret) ->
              let arg = elab_exp env level arg in
              unify param arg ; ret
          | _ -> error "expected function" )
        | Bool _ -> i1
        | Number _ -> i64 (* TODO: implement integer suffix  *)
        | Text _ -> text
        | Unit -> unit
        | BinOp (left, op, right) -> (
          (* TODO: many of binary operators are planned to be desugared to a call to type class method *)
          match op with
          | Add | Substract | Multiply | Division | Xor | Reminder ->
              unify i64 (elab left) ;
              unify i64 (elab right) ;
              i64
          | BitwiseLeftShift | BitwiseRightShift | BitwiseAnd | BitwiseOr ->
              unify u64 (elab left) ;
              unify u64 (elab right) ;
              i64
          | Less | Greater ->
              unify i64 (elab left) ;
              unify i64 (elab right) ;
              i1
          | Equal | NotEqual ->
              unify (elab left) (elab right) ;
              i1
          | And | Or ->
              unify i1 (elab left) ;
              unify i1 (elab right) ;
              i1
          | Combine | Remove | Cons -> noimpl "sequence operators"
          | Pipeline -> noimpl "pipeline operator"
          | Append | Prepend | Erase ->
              noimpl "sequence-with-element operators"
          | Dot -> noimpl "dot operator"
          | AddAsign | SubstractAsign | Asign -> noimpl "assignment operators"
          )
        | PrefixOp (op, operand) -> (
            let operandType = elab operand in
            match op with
            | Positive | Negative -> unify i64 operandType ; i64
            | Not -> unify i1 operandType ; i1
            | BitwiseNot -> unify u64 operandType ; u64
            | Deref | Ref -> noimpl "ref/deref operators"
            | Raise -> noimpl "raise"
            | Lazy -> noimpl "lazy"
            | PrefixIncrement | PrefixDecrement ->
                noimpl "increment/decrement operators" )
        | PostfixOp _ -> noimpl "postfix operators"
        | Tuple values -> TProduct (List.map elab values)
        | ListLiteral values ->
            let types = List.map elab values in
            let elemType = new_var level in
            List.iter (unify elemType) types ;
            TApp ([elemType], listType)
        | ArrayLiteral values ->
            let types = List.map elab values in
            let elemType = new_var level in
            List.iter (unify elemType) types ;
            TApp ([elemType], arrayType)
        | IfThenElse (cond, body, Some fallback) ->
            elab cond |> unify i1 ;
            let bodyType = elab body in
            let fallbackType = elab fallback in
            unify bodyType fallbackType ;
            bodyType
        | IfThenElse (cond, body, None) ->
            elab cond |> unify i1 ;
            elab body |> ignore ;
            unit
        | Seq exps ->
            let rec aux = function
              | [last] -> elab last
              | exp :: t ->
                  elab exp |> ignore ;
                  aux t
              | [] -> unit
            in
            aux exps
        | Match _ -> noimpl "pattern matching"
        | Handle _ -> noimpl "effect handler"
        | RecordLiteral _ | RecordSelection _ | RecordRestrictionLiteral _ ->
            noimpl "record"
        | PolymorphicVariantConstruction _ -> noimpl "polymorphic variant"
      in
      set_ty data ret ; ret

let elab_module_part path _ =
  function
  | _, part -> (
      (* prepare *)
      let env = empty in
      let level = 0 in
      match part with
      | TypeDeclInModule ((_, MonomorphicVariant (params, name, ctors)) as decl)
        ->
          let ctor_types =
            List.map
              (function
                | (_, Ctor (name, type_exp)) as ctor ->
                    let ty = elab_type env type_exp in
                    set_ty_of ctor ty ;
                    (name, elab_type env type_exp))
              ctors
          in
          let ty = TVariant (params, name, ctor_types) in
          set_ty_of decl ty ;
          let env = extend_type env (LongId (List.append path [name])) decl in
          let rec aux env = function
            | ((_, Ctor (name, _)) as ctor) :: t ->
                aux
                  (extend_value env
                     (LongId (List.append path [name]))
                     (CtorValue ctor))
                  t
            | [] -> env
          in
          aux env ctors
      | TypeDeclInModule _ -> noimpl "interface"
      | LetDef (((_, PIdent name) as bindant), exp) ->
          let exp_type = elab_exp env (level + 1) exp in
          let generalized_type = generalize level exp_type in
          set_ty_of bindant generalized_type ;
          extend_value env
            (LongId (List.append path [name]))
            (Captured bindant)
      | LetDef ((_, _), _) -> noimpl "pattern binding"
      | LetRecDef lets ->
          let rec allocate_type_vars env = function
            | (_, LetRecDefPart (((_, PIdent name) as bindant), _))
              :: t ->
                let ty = new_var (level + 1) in
                set_ty_of bindant ty ;
                allocate_type_vars
                  (extend_value env (LongId [name]) (Captured bindant))
                  t
            | (_, _) :: _ -> noimpl "pattern binding"
            | [] -> env
          in
          let env = allocate_type_vars env lets in
          let rec elabParts env = function
            | (_, LetRecDefPart (_, exp)) :: t ->
                elab_exp env (level + 1) exp |> ignore ;
                elabParts env t
            | [] -> env
          in
          elabParts env lets )

let rec elab_module' path env type_env module_id name = function
  | h :: t ->
      let env = elab_module_part path env h in
      elab_module' path env type_env module_id name t
  | [] -> env

let elab_module module_id name module_parts =
  elab_module' [name] empty empty module_id name module_parts

let elab_decl = function
  | data, decl -> (
    match decl with
    | InterfaceDecl _ -> noimpl "interface"
    | FunctorDecl _ -> noimpl "functor"
    | ModuleDecl (Some name, false, (module_parts, [])) ->
        elab_module data name module_parts
    | ModuleDecl (Some _, false, (_, _ :: _)) -> noimpl "interface"
    | ModuleDecl (_, true, _) -> noimpl "given module"
    | ModuleDecl (None, _, _) -> noimpl "anonymous module" )