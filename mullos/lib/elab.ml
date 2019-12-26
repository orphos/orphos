(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki3478@res.ac>
 *
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
open Aux
open Syntax

type data = { mutable ty_field : Syntax.Type.ty option; oid : oid }

let get_ty = function { ty_field = Some ty; _ } -> ty | { ty_field = None; _ } -> bug "ty is not allocated yet"

let set_ty data ty = data.ty_field <- Some ty

let get_ty_of = function data, _ -> get_ty data

let set_ty_of = function data, _ -> fun ty -> data.ty_field <- Some ty

module ElabData = struct
  type t = data

  let allocate () = { ty_field = None; oid = new_oid () }
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
      | [], _ :: _ -> -1
    in
    let LongId left, LongId right = (left, right) in
    aux (left, right)
end

module IdMap = Map.Make (LongIdIsOrdered)

type value = Exp of exp | CtorValue of ctor | Captured of pat

let get_ty_of_value = function Exp (data, _) | CtorValue (data, _) | Captured (data, _) -> get_ty data

type env = { values : (long_id, value) Env.t; types : (long_id, type_decl) Env.t }

let enter env =
  Env.enter env.values;
  Env.enter env.types

let leave env =
  Env.leave env.values;
  Env.leave env.types

let empty () : env = { values = Env.create (); types = Env.create () }

let extend_value env name value = Env.put env.values name value

let extend_type env name type_exp = Env.put env.types name type_exp

let resolve_value env name =
  match Env.lookup env.values name with None -> error ("Could not resolve " ^ show_long_id name) | Some ret -> ret

let resolve_type env name =
  match Env.lookup env.types name with None -> error ("Coudl not resolve " ^ show_long_id name) | Some ret -> ret

let occurs_check_adjust_levels tvar_id tvar_level ty =
  let rec f = function
    | TVar { contents = Link ty } -> f ty
    | TVar { contents = Generic _ } -> bug "occurs check to generic variable"
    | TVar ({ contents = Unbound (other_id, other_level) } as other_tvar) ->
        if other_id = tvar_id then error "recursive types"
        else if other_level > tvar_level then other_tvar := Unbound (other_id, tvar_level)
        else ()
    | TApply (ty_args, ty) ->
        f ty;
        List.iter f ty_args
    | TArrow (param_ty, return_ty) ->
        f param_ty;
        f return_ty
    | TLongId _ -> ()
    | TTuple tys -> List.iter f tys
    | TVariant (_, _, ctors) -> List.iter (function _, ty -> f ty) ctors
    | TRecord row -> f row
    | TRowEmpty -> ()
    | TRowExtend (_, ty, rest) ->
        f ty;
        f rest
    | TGiven (value, row) ->
        f value;
        f row
  in
  f ty

let rec unify ty1 ty2 =
  if ty1 == ty2 then ()
  else
    match (ty1, ty2) with
    | TLongId long_id1, TLongId long_id2 when long_id1 = long_id2 -> ()
    | TApply (ty_args1, ty1), TApply (ty_args2, ty2) ->
        unify ty1 ty2;
        List.iter2 unify ty_args1 ty_args2
    | TArrow (param_ty1, return_ty1), TArrow (param_ty2, return_ty2) ->
        unify param_ty1 param_ty2;
        unify return_ty1 return_ty2
    | TVar { contents = Link ty1 }, ty2 | ty1, TVar { contents = Link ty2 } -> unify ty1 ty2
    | TVar { contents = Unbound (id1, _) }, TVar { contents = Unbound (id2, _) } when id1 = id2 ->
        bug "multiple instance of a particular type variable."
    | TVar ({ contents = Unbound (id, level) } as tvar), ty | ty, TVar ({ contents = Unbound (id, level) } as tvar) ->
        occurs_check_adjust_levels id level ty;
        tvar := Link ty
    | TTuple xs, TTuple ys -> List.iter2 unify xs ys
    | TRecord left, TRecord right -> unify left right
    | TRowEmpty, TRowEmpty -> ()
    | TRowExtend (label1, ty1, rest1), (TRowExtend _ as row2) ->
        let rest_row1_tvar_ref_option =
          match rest1 with TVar ({ contents = Unbound _ } as tvar_ref) -> Some tvar_ref | _ -> None
        in
        let rec rewrite_row label1 ty1 = function
          | TRowEmpty -> "row does not contain label " ^ Type.oid_to_label label1 |> error
          | TRowExtend (label2, ty2, rest2) when label2 = label1 ->
              unify ty1 ty2;
              rest2
          | TRowExtend (label2, ty2, rest2) -> TRowExtend (label2, ty2, rewrite_row label1 ty1 rest2)
          | TVar { contents = Link row2 } -> rewrite_row label1 ty1 row2
          | TVar ({ contents = Unbound (_, level) } as tvar) ->
              let rest2 = new_var level in
              let ty2 = TRowExtend (label1, ty1, rest2) in
              tvar := Link ty2;
              rest2
          | _ -> error "row type exepcted"
        in
        let rest2 = rewrite_row label1 ty1 row2 in
        (match rest_row1_tvar_ref_option with Some { contents = Link _ } -> error "recursive row types" | _ -> ());
        unify rest1 rest2
    | TGiven (value1, row1), TGiven (value2, row2) ->
        unify value1 value2;
        unify row1 row2
    | TGiven (value1, _), value2 | value1, TGiven (value2, _) ->
        (* TODO: check type class constraint *)
        unify value1 value2
    | _, _ -> error "cannot unify types "

let rec generalize level = function
  | TVar { contents = Unbound (id, other_level) } when other_level > level -> TVar (ref (Generic id))
  | TApply (ty_args, ty) -> TApply (List.map (generalize level) ty_args, generalize level ty)
  | TArrow (param_ty, return_ty) -> TArrow (generalize level param_ty, generalize level return_ty)
  | TTuple tys -> TTuple (List.map (generalize level) tys)
  | TVariant (params, name, ctors) ->
      TVariant (params, name, List.map (function name, ty -> (name, generalize level ty)) ctors)
  | TVar { contents = Link ty } -> generalize level ty
  | (TVar { contents = Generic _ } | TVar { contents = Unbound _ } | TLongId _) as ty -> ty
  | TRecord row -> TRecord (generalize level row)
  | TRowEmpty -> TRowEmpty
  | TRowExtend (label, ty, rest) -> TRowExtend (label, generalize level ty, generalize level rest)
  | TGiven (value, row) -> TGiven (generalize level value, generalize level row)

let instantiate level ty =
  let id_var_map = Hashtbl.create 10 in
  let rec f ty =
    match ty with
    | TLongId _ -> ty
    | TVar { contents = Link ty } -> f ty
    | TVar { contents = Generic id } -> (
        try Hashtbl.find id_var_map id
        with Not_found ->
          let var = new_var level in
          Hashtbl.add id_var_map id var;
          var )
    | TVar { contents = Unbound _ } -> ty
    | TApply (ty_args, ty) -> TApply (List.map f ty_args, f ty)
    | TArrow (param_ty, return_ty) -> TArrow (f param_ty, f return_ty)
    | TTuple tys -> TTuple (List.map f tys)
    | TVariant (params, name, ctors) -> TVariant (params, name, List.map (function name, ty -> (name, f ty)) ctors)
    | TRecord row -> TRecord (f row)
    | TRowEmpty -> TRowEmpty
    | TRowExtend (label, ty, rest) -> TRowExtend (label, f ty, f rest)
    | TGiven (value, row) -> TGiven (f value, f row)
  in
  f ty

let rec elab_type env = function
  | data, type_exp ->
      let elab = elab_type env in
      let ty =
        match type_exp with
        | EIdent id -> resolve_type env id |> get_ty_of
        | EGeneric id -> resolve_type env (long_id [ id ]) |> get_ty_of
        | ELazy _ -> noimpl "lazy"
        | ELabel _ -> noimpl "record"
        | EEff _ -> noimpl "effect"
        | ERecord _ -> noimpl "record"
        | EPolymorphicVariant _ -> noimpl "polymorphic variant"
        | EOr _ -> noimpl "union type"
        | ERefinement _ -> noimpl "refinement type"
        | EGiven _ -> noimpl "given type"
        | EArrow (param, ret) -> TArrow (elab param, elab ret)
        | ETuple elems -> TTuple (elems |> List.map elab)
        | EApply (params, applicant) -> TApply (params |> List.map elab, elab applicant)
      in
      set_ty data ty;
      ty

let rec elab_pat (env : env) (level : level) = function
  | data, pat' ->
      let ty =
        match pat' with
        | PIdent _ -> new_var level
        | PUnit -> TLongId (LongId [ "unit" ])
        | PCapture (_, pat) -> elab_pat env level pat
        | PCtor _ -> noimpl "variant constructor pattern"
        | PTuple pats -> TTuple (List.map (fun pat -> elab_pat env level pat) pats)
        | PWildcard -> new_var level
        | PText _ -> TLongId (LongId [ "text" ])
        | PNumber _ -> TLongId (LongId [ "i32" ])
        | PBool _ -> TLongId (LongId [ "bool" ])
        | PLazy _ -> noimpl "lazy pattern"
        | POr (pat1, pat2) ->
            unify (elab_pat env level pat1) (elab_pat env level pat2);
            noimpl "union pattern"
        | PListLiteral _ -> noimpl "list pattern"
        | PArrayLiteral _ -> noimpl "array pattern"
        | PPolymorphicVariant _ -> noimpl "polymorphic variant pattern"
      in
      set_ty data ty;
      ty

let rec elab_exp env level = function
  | data, exp' ->
      enter env;
      let elab = elab_exp env level in
      let ret =
        match exp' with
        | Ident name -> (
            try instantiate level (resolve_value env name |> get_ty_of_value)
            with Not_found -> error "variable not found" )
        | Lambda (((param_data, PIdent param_name) as param), body) ->
            let param_ty = new_var level in
            set_ty param_data param_ty;
            extend_value env (LongId [ param_name ]) (Captured param);
            let return_ty = elab_exp env level body in
            TArrow (param_ty, return_ty)
        | Lambda ((_, _), _) -> noimpl "pattern param"
        | Let (((_, PIdent name) as bindant), _, value, body) ->
            let value_ty = elab_exp env (level + 1) value in
            let generalized_ty = generalize level value_ty in
            set_ty data generalized_ty;
            extend_value env (LongId [ name ]) (Captured bindant);
            elab_exp env level body
        | Let _ -> noimpl "binding to pattern"
        | LetRec (lets, body) ->
            let rec allocate_type_vars = function
              | (((pat_data, PIdent name) as bindant), _, _) :: t ->
                  let ty = new_var (level + 1) in
                  set_ty pat_data ty;
                  extend_value env (LongId [ name ]) (Captured bindant);
                  allocate_type_vars t
              | ((_, _), _, _) :: _ -> noimpl "binding to pattern"
              | [] -> ()
            in
            allocate_type_vars lets;
            let rec elabBindees = function
              | (((pat_data, PIdent name) as bindant), _, value) :: t ->
                  let value_ty = elab_exp env (level + 1) value in
                  set_ty pat_data value_ty;
                  extend_value env (LongId [ name ]) (Captured bindant);
                  elabBindees t
              | [] -> ()
              | ((_, _), _, _) :: _ -> noimpl "binding to pattern"
            in
            elabBindees lets;
            (* elab_exp body *)
            elab_exp env level body
        | Apply (fn, arg) -> (
            match elab_exp env level fn with
            | TArrow (param, ret) ->
                let arg = elab_exp env level arg in
                unify param arg;
                ret
            | _ -> error "expected function" )
        | Bool _ -> i1
        | Number _ -> i64 (* TODO: implement integer suffix *)
        | Text _ -> text
        | Unit -> unit
        | BinOp (left, op, right) -> (
            (* TODO: many of binary operators are planned to be desugared to a call to type class method *)
            match op with
            | Add | Substract | Multiply | Division | Xor | Reminder ->
                unify i64 (elab left);
                unify i64 (elab right);
                i64
            | BitwiseLeftShift | BitwiseRightShift | BitwiseAnd | BitwiseOr ->
                unify u64 (elab left);
                unify u64 (elab right);
                i64
            | Less | Greater ->
                unify i64 (elab left);
                unify i64 (elab right);
                i1
            | Equal | NotEqual ->
                unify (elab left) (elab right);
                i1
            | And | Or ->
                unify i1 (elab left);
                unify i1 (elab right);
                i1
            | Combine | Remove | Cons -> noimpl "sequence operators"
            | Append | Prepend | Erase -> noimpl "sequence-with-element operators"
            | Dot -> noimpl "dot operator"
            | AddAsign | SubstractAsign | Asign -> noimpl "assignment operators" )
        | PrefixOp (op, operand) -> (
            let operandType = elab operand in
            match op with
            | Positive | Negative ->
                unify i64 operandType;
                i64
            | Not ->
                unify i1 operandType;
                i1
            | BitwiseNot ->
                unify u64 operandType;
                u64
            | Deref | Ref -> noimpl "ref/deref operators"
            | Raise -> noimpl "raise"
            | Lazy -> noimpl "lazy"
            | PrefixIncrement | PrefixDecrement -> noimpl "increment/decrement operators" )
        | PostfixOp _ -> noimpl "postfix operators"
        | Tuple values -> TTuple (List.map elab values)
        | ListLiteral values ->
            let types = List.map elab values in
            let elemType = new_var level in
            List.iter (unify elemType) types;
            TApply ([ elemType ], listType)
        | ArrayLiteral values ->
            let types = List.map elab values in
            let elemType = new_var level in
            List.iter (unify elemType) types;
            TApply ([ elemType ], arrayType)
        | IfThenElse (cond, body, Some fallback) ->
            elab cond |> unify i1;
            let bodyType = elab body in
            let fallbackType = elab fallback in
            unify bodyType fallbackType;
            bodyType
        | IfThenElse (cond, body, None) ->
            elab cond |> unify i1;
            elab body |> ignore;
            unit
        | Seq exps ->
            let rec aux = function
              | [ last ] -> elab last
              | exp :: t ->
                  elab exp |> ignore;
                  aux t
              | [] -> unit
            in
            aux exps
        | Match _ -> noimpl "pattern matching"
        | Handle _ -> noimpl "effect handler"
        | RecordEmpty -> TRecord TRowEmpty
        | RecordExtend (rest, label, value) ->
            let rest_type = new_var level in
            let label_type = new_var level in
            let ret_type = TRecord (TRowExtend (Type.label_to_oid label, label_type, rest_type)) in
            unify label_type (elab_exp env level value);
            unify (TRecord rest_type) (elab_exp env level rest);
            TRecord ret_type
        | RecordSelection (record, label) ->
            let rest_type = new_var level in
            let label_type = new_var level in
            let record_type = TRecord (TRowExtend (Type.label_to_oid label, label_type, rest_type)) in
            unify record_type (elab_exp env level record);
            label_type
        | RecordRestrictionLiteral (record, label) ->
            let rest_type = new_var level in
            let label_type = new_var level in
            let record_type = TRowExtend (Type.label_to_oid label, label_type, rest_type) in
            unify (TRecord record_type) (elab_exp env level record);
            rest_type
        | PolymorphicVariantConstruction _ -> noimpl "polymorphic variant"
        | Construct _ -> noimpl "monomorphic variant"
      in
      set_ty data ret;
      leave env;
      ret

let elab_module_part path _ = function
  | _, part -> (
      (* prepare *)
      let env = empty () in
      let level = 0 in
      match part with
      | TypeDeclInModule ((_, MonomorphicVariant (params, name, ctors)) as decl) ->
          let ctor_types =
            List.map
              (function
                | (_, Ctor (name, type_exp)) as ctor ->
                    let ty = elab_type env type_exp in
                    set_ty_of ctor ty;
                    (name, elab_type env type_exp))
              ctors
          in
          let ty = TVariant (params, name, ctor_types) in
          set_ty_of decl ty;
          extend_type env (LongId (List.append path [ name ])) decl;
          let rec aux = function
            | ((_, Ctor (name, _)) as ctor) :: t ->
                extend_value env (LongId (List.append path [ name ])) (CtorValue ctor);
                aux t
            | [] -> ()
          in
          aux ctors
      | TypeDeclInModule _ -> noimpl "interface"
      | LetDef (((_, PIdent name) as bindant), exp) ->
          let exp_type = elab_exp env (level + 1) exp in
          let generalized_type = generalize level exp_type in
          set_ty_of bindant generalized_type;
          extend_value env (LongId (List.append path [ name ])) (Captured bindant)
      | LetDef ((_, _), _) -> noimpl "pattern binding"
      | LetRecDef lets ->
          let rec allocate_type_vars = function
            | (_, LetRecDefPart (((_, PIdent name) as bindant), _)) :: t ->
                let ty = new_var (level + 1) in
                set_ty_of bindant ty;
                extend_value env (LongId [ name ]) (Captured bindant);
                allocate_type_vars t
            | (_, _) :: _ -> noimpl "pattern binding"
            | [] -> ()
          in
          allocate_type_vars lets;
          let rec elabParts = function
            | (_, LetRecDefPart (_, exp)) :: t ->
                elab_exp env (level + 1) exp |> ignore;
                elabParts t
            | [] -> ()
          in
          elabParts lets )

let elab_module' path env _ _ decls = List.iter (elab_module_part path env) decls

let elab_module module_id name module_parts env =
  enter env;
  elab_module' [ name ] (empty ()) module_id name module_parts

let elab_decl = function
  | data, decl -> (
      match decl with
      | InterfaceDecl _ -> noimpl "interface"
      | FunctorDecl _ -> noimpl "functor"
      | ModuleDecl (Some name, false, (module_parts, [])) -> elab_module data name module_parts (empty ())
      | ModuleDecl (Some _, false, (_, _ :: _)) -> noimpl "interface"
      | ModuleDecl (_, true, _) -> noimpl "given module"
      | ModuleDecl (None, _, _) -> noimpl "anonymous module" )
