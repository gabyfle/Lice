(*****************************************************************************)
(*                                                                           *)
(*  This file is part of Lice.                                               *)
(*                                                                           *)
(*  Copyright (C) 2023                                                       *)
(*    Gabriel Santamaria                                                     *)
(*                                                                           *)
(*                                                                           *)
(*  Licensed under the Apache License, Version 2.0 (the "License");          *)
(*  you may not use this file except in compliance with the License.         *)
(*  You may obtain a copy of the License at                                  *)
(*                                                                           *)
(*    http://www.apache.org/licenses/LICENSE-2.0                             *)
(*                                                                           *)
(*  Unless required by applicable law or agreed to in writing, software      *)
(*  distributed under the License is distributed on an "AS IS" BASIS,        *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *)
(*  See the License for the specific language governing permissions and      *)
(*  limitations under the License.                                           *)
(*                                                                           *)
(*****************************************************************************)

open Ast.Tree
open Types
open Types.Base
open Env
open Located_error

module type EVAL = sig
  val exec : program -> unit
end

module Eval : EVAL = struct
  type _expr_or_statement = [`Expression of expr | `Statement of statement]

  let get_type env loc id =
    match Scope.get env id with
    | Some (Expression (_, expr, T_Auto)) ->
        val_to_typ expr
    | Some (Expression (_, _, t)) ->
        t
    | _ ->
        raise (Located_error (`Undefined_Variable id, loc))

  let is_variable_type env loc id t =
    try
      let t' = get_type env loc id in
      t = t'
    with Located_error (`Undefined_Variable _, _) -> false

  let get_value env loc ident =
    match Scope.get env ident with
    | None ->
        raise (Located_error (`Undefined_Variable ident, loc))
    | Some h -> (
      match h with
      | Expression (_, e, _) ->
          `Expression e
      | FuncDef (_, _, _, _) as f ->
          `Statement f
      | _ ->
          let str =
            Printf.sprintf "Variable %s definition error."
              (identificator_to_string ident)
          in
          raise (Located_error (`Language_Error str, loc)) )

  (* from a module name and an id of a variable, returns the correct version of
     the naming this is used to add the variable inside the correct
     environnement when accessing it *)
  let update_name_module in_mod id =
    match in_mod with
    | None ->
        id
    | Some m -> (
      match id with `Ident ((_, _) as id) -> `Module (m, id) | `Module _ -> id )

  let rec binop_helper env loc ?(in_mod = None) v v' op =
    let aux a b op =
      let value =
        match op with
        | Plus ->
            Value.add a b
        | Minus ->
            Value.sub a b
        | Multiply ->
            Value.mul a b
        | Divide ->
            Value.div a b
        | Mod ->
            Value.md a b
      in
      Terminal (Base.Const value)
    in
    match (v, v') with
    | Terminal (Const k), Terminal (Const k') ->
        aux k k' op
    | Terminal (Const k), Terminal (V_Var id) ->
        let id = update_name_module in_mod id in
        let t_typ = Value.to_typ (Base.Const k) in
        let same_type = is_variable_type env loc id t_typ in
        if same_type then
          let value =
            match get_value env loc id with
            | `Expression (Terminal (Const k')) ->
                k'
            | _ ->
                raise (Located_error (`Function_Value, loc))
          in
          aux k value op
        else
          raise
            (Located_error
               ( `Op_Mismatch (binop_to_string op, t_typ, get_type env loc id)
               , loc ) )
    | Terminal (V_Var id), Terminal (Const k) ->
        let id = update_name_module in_mod id in
        let t_typ = Value.to_typ (Base.Const k) in
        let same_type = is_variable_type env loc id t_typ in
        if same_type then
          let value =
            match get_value env loc id with
            | `Expression (Terminal (Const k')) ->
                k'
            | _ ->
                raise (Located_error (`Not_Number, loc))
          in
          aux value k op
        else raise (Located_error (`Not_Number, loc))
    (* as a quick fix for the moment (maybe we will be able to do this
       differently in near future) we need a special canse to handle h :: t
       operator *)
    | Terminal (V_Var id), Terminal (V_Var id') ->
        let id = update_name_module in_mod id in
        let id' = update_name_module in_mod id' in
        let same_id = id = id' in
        let t = get_type env loc id in
        let t' = get_type env loc id' in
        let same_type = t = t' in
        if same_type then
          let v = get_value env loc id in
          let v' = if same_id then v else get_value env loc id' in
          match (v, v') with
          | `Expression (Terminal (Const k)), `Expression (Terminal (Const k'))
            ->
              aux k k' op
          | _ ->
              raise (Located_error (`Function_Value, loc))
        else
          raise (Located_error (`Op_Mismatch (binop_to_string op, t, t'), loc))
    | a, b ->
        let tmp, v_a = eval_expr env loc ~in_mod (env, a) in
        let tmp', v_b = eval_expr tmp loc ~in_mod (tmp, b) in
        binop_helper tmp' loc ~in_mod v_a v_b op

  and bincomp_helper env loc ?(in_mod = None) v v' =
    let aux a b op =
      let value =
        match op with
        | Equal -> (
            let res = Value.eq a b in
            match res with
            | V_Boolean b ->
                b
            | _ ->
                raise (Located_error (`Not_Number, loc)) )
        | NotEqual -> (
            let res = Value.eq a b in
            match res with
            | V_Boolean b ->
                not b
            | _ ->
                raise (Located_error (`Not_Number, loc)) )
        | GEQ ->
            Value.compare a b >= 0
        | LEQ ->
            Value.compare a b <= 0
        | Greater ->
            Value.compare a b > 0
        | Lesser ->
            Value.compare a b < 0
      in
      Terminal (Const (V_Boolean value))
    in
    match (v, v') with
    | Terminal (Const k), Terminal (Const k') ->
        aux k k'
    | Terminal (V_Var id), Terminal (Const k)
    | Terminal (Const k), Terminal (V_Var id) ->
        let id = update_name_module in_mod id in
        let t_typ = Value.to_typ (Base.Const k) in
        let same_type = is_variable_type env loc id t_typ in
        if same_type then
          let value =
            match get_value env loc id with
            | `Expression (Terminal (Const k')) ->
                k'
            | _ ->
                raise (Located_error (`Not_Number, loc))
          in
          aux value k
        else raise (Located_error (`Not_Number, loc))
    | Terminal (V_Var id), Terminal (V_Var id') ->
        let id = update_name_module in_mod id in
        let id' = update_name_module in_mod id' in
        let same_id = id = id' in
        let same_type = get_type env loc id = get_type env loc id' in
        if same_type then
          let v = get_value env loc id in
          let v' = if same_id then v else get_value env loc id' in
          match (v, v') with
          | `Expression (Terminal (Const k)), `Expression (Terminal (Const k'))
            ->
              aux k k'
          | _ ->
              raise (Located_error (`Not_Number, loc))
        else raise (Located_error (`Not_Number, loc))
    | a, b ->
        let tmp, v_a = eval_expr env loc ~in_mod (env, a) in
        let tmp', v_b = eval_expr tmp loc ~in_mod (tmp, b) in
        bincomp_helper tmp' loc ~in_mod v_a v_b

  and cons_helper env loc ?(in_mod = None) a b =
    match (a, b) with
    | Terminal (V_Var id), (Terminal (V_Var _) as k') -> (
        let value = get_value env loc id in
        match value with
        | `Expression k ->
            binop_helper env loc ~in_mod
              (Terminal (Const (V_List (Llist.from k))))
              k' Plus
        | _ ->
            raise (Located_error (`Not_Number, loc)) )
    | _ ->
        binop_helper env loc ~in_mod a b Plus

  (* eval_function evaluates a function call inside the AST to remplace it by
     the actual value returned (or not) by the function. this function processes
     the given arguments first, then populates a new scope made out of the
     current one but with the new local variables defined in the function
     definition *)
  and eval_function env ?(in_mod = None) loc id expr_list =
    let id = update_name_module in_mod id in
    let in_mod =
      match id with `Ident _ -> in_mod | `Module (n, (_, _)) -> Some n
    in
    let params, stmts =
      match get_value env loc id with
      | `Statement (FuncDef (_, _, p, Block (_, s))) ->
          (p, s)
      | _ ->
          raise (Located_error (`Not_A_Callable, loc))
    in
    let rec process_args acc = function
      (* we first need to process every argument in order to pass them to the
         function *)
      | [] ->
          acc
      | h :: t ->
          process_args (eval_expr env loc ~in_mod h :: acc) t
    in
    let processed_args = List.rev (process_args [] expr_list) in
    (* f_env is a copy of our current env *)
    (* then we want to populate the new env with these processed arguments so
       that inside the function we got our local variables *)
    let rec add_to_scope acc a b =
      match (a, b) with
      | [], [] ->
          acc
      | [], _ | _, [] ->
          raise
            (Located_error
               ( `Wrong_Parameters_Number
                   (id, List.length params, List.length processed_args)
               , loc ) )
      | id :: t, (_, e) :: t' ->
          add_to_scope (Scope.set acc id (Expression (loc, e, T_Auto))) t t'
    in
    let f_env = add_to_scope (Scope.push_scope env) params processed_args in
    (* now we can evaluate the function statements *)
    let rec aux env = function
      | [] ->
          raise (Located_error (`Expected_Return_Statement, loc))
      | stmt :: t -> (
          let n_env, stmt = eval_statement env ~in_mod stmt in
          match stmt with
          | Return (_, e) ->
              let expected_ret =
                Scope.get env id
                |> function
                | Some (FuncDef (_, id, _, _)) -> (
                    let id = update_name_module in_mod id in
                    match id with
                    | `Ident (_, t) ->
                        t
                    | `Module (_, (_, t)) ->
                        t )
                | _ ->
                    raise (Located_error (`Not_A_Callable, loc))
              in
              let scp, processed = eval_expr n_env loc ~in_mod (n_env, e) in
              (* check the return type of the processed return expression *)
              let return_type = val_to_typ processed in
              if expected_ret <> T_Auto && return_type <> expected_ret then
                raise
                  (Located_error (`Wrong_Type (expected_ret, return_type), loc))
              else (Scope.pop_scope scp, processed)
          | _ ->
              aux n_env t )
    in
    aux f_env stmts

  and eval_expr _env loc ?(in_mod = None) = function
    | env, Terminal (Const V_Void) ->
        (env, Terminal (Const V_Void))
    | ( _
      , Terminal
          (Const
            (V_Number _ | V_String _ | V_Boolean _ | V_List _ | V_Function _) )
      ) as v ->
        v
    | env, Terminal (V_Var id) -> (
        let id = update_name_module in_mod id in
        match get_value env loc id with
        | `Expression e ->
            (env, e)
        | `Statement _ ->
            raise
              (Located_error
                 ( `Language_Error
                     "An error occurred while trying to get the variable"
                 , loc ) ) )
    | env, BinOp (op, a, b) -> (
      match op with
      | `Compare bincomp ->
          (env, bincomp_helper env loc ~in_mod a b bincomp)
      | `Operator binop ->
          (env, binop_helper env loc ~in_mod a b binop)
      | `Cons ->
          (env, cons_helper env loc ~in_mod a b) )
    | env, FuncCall (id, expr_list) ->
        let id = update_name_module in_mod id in
        let rec exprs acc = function
          | [] ->
              List.rev acc
          | h :: t ->
              exprs ((env, h) :: acc) t
        in
        eval_function env ~in_mod loc id (exprs [] expr_list)

  and eval_match env ?(in_mod = None) loc pattern cases =
    let n_env, eval_pattern = eval_expr env loc pattern in
    let rec iterate_cases = function
      | [] ->
          (n_env, Expression (loc, Terminal (Const V_Void), T_Void))
      (* if we find a wildcard then we can stop analysis of the match
         statement *)
      | (Terminal (Const V_Void), stmts) :: _ ->
          eval_statement n_env (Block (loc, stmts))
      (* when encountering list destructuring, we need to bind muted variables
         into their actual values, then execute the code *)
      | ( BinOp (`Cons, (Terminal (Const (V_List _)) as hd), Terminal (V_Var id))
        , stmts )
        :: _ ->
          let id = update_name_module in_mod id in
          let hd' =
            match eval_pattern with
            | Terminal (Const (V_List k)) -> (
              match Llist.hd k with
              | None ->
                  Terminal (Const V_Void)
              | Some k ->
                  k )
            | _ ->
                raise (Located_error (`Wrong_Type (T_List, T_Auto), loc))
          in
          if Value.expr_eq hd hd' then
            (* we ensure that both heads are equal before doing anything *)
            let tl =
              match eval_pattern with
              | Terminal (Const (V_List k)) -> (
                match Llist.tl k with None -> V_List [] | Some k -> V_List k )
              | _ ->
                  raise (Located_error (`Wrong_Type (T_List, T_Auto), loc))
            in
            let value = Terminal (Const tl) in
            let tmp = Scope.push_scope n_env in
            let n_env = Scope.set tmp id (Expression (loc, value, T_Auto)) in
            eval_statement n_env (Block (loc, stmts))
          else iterate_cases (List.tl cases)
      (* we encountered a case where it's destructuring the list like this: h ::
         t *)
      | (BinOp (`Cons, Terminal (V_Var id), Terminal (V_Var id')), stmts) :: _
        -> (
          let id = update_name_module in_mod id in
          let id' = update_name_module in_mod id' in
          let hd =
            match eval_pattern with
            | Terminal (Const (V_List k)) -> (
              match Llist.hd k with
              | None ->
                  Terminal (Const V_Void)
              | Some k ->
                  k )
            | _ ->
                raise (Located_error (`Wrong_Type (T_List, T_Auto), loc))
          in
          match hd with
          | Terminal (Const V_Void) ->
              iterate_cases (List.tl cases)
          | _ ->
              let tl =
                match eval_pattern with
                | Terminal (Const (V_List k)) -> (
                  match Llist.tl k with None -> V_List [] | Some k -> V_List k )
                | _ ->
                    raise (Located_error (`Wrong_Type (T_List, T_Auto), loc))
              in
              let value = Terminal (Const tl) in
              let tmp = Scope.push_scope n_env in
              let n_env = Scope.set tmp id (Expression (loc, hd, T_Auto)) in
              let n_env =
                Scope.set n_env id' (Expression (loc, value, T_Auto))
              in
              eval_statement n_env (Block (loc, stmts)) )
      | (case, stmts) :: _ -> (
          let n_env, eval_case = eval_expr n_env loc (n_env, case) in
          match (eval_case, eval_pattern) with
          | Terminal (Const k), Terminal (Const k') ->
              let eq =
                match Value.eq k k' with V_Boolean b -> b | _ -> false
              in
              if eq then eval_statement n_env (Block (loc, stmts))
              else iterate_cases (List.tl cases)
          | _ ->
              raise (Located_error (`Wrong_Type (T_Auto, T_Auto), loc)) )
    in
    iterate_cases cases

  and eval_statement env ?(in_mod = None) = function
    | Return (loc, e) ->
        let env, e = eval_expr env ~in_mod loc (env, e) in
        (env, Return (loc, e))
        (* Wrap the expression in a Return statement *)
    | Expression (loc, e, _typ) ->
        let env, expr = eval_expr env ~in_mod loc (env, e) in
        (env, Expression (loc, expr, _typ))
        (* Wrap the expression in an Expression statement *)
    | Block (loc, stmts) ->
        let blck_env = Scope.push_scope env in
        let rec aux env = function
          | [] ->
              ( Scope.pop_scope env
              , Expression (loc, Terminal (Const V_Void), T_Void) )
          | stmt :: t -> (
              let nenv, stmt = eval_statement env ~in_mod stmt in
              match stmt with
              | Return _ ->
                  (nenv, stmt)
                  (* If the statement is a Return statement, immediately return
                     it *)
              | _ ->
                  aux nenv t (* Otherwise, continue with the next statement *) )
        in
        aux blck_env stmts
    | Assign (loc, id, e) ->
        let t = Value.get_typ_from_id id in
        let tmp, processed = eval_expr env ~in_mod loc (env, e) in
        if t <> T_Auto && t <> val_to_typ processed then
          raise (Located_error (`Wrong_Type (t, val_to_typ processed), loc))
        else
          let id' = update_name_module in_mod id in
          let n_env =
            Scope.set tmp id' (Expression (loc, processed, val_to_typ processed))
          in
          (n_env, Expression (loc, Terminal (Const V_Void), T_Void))
    | FuncDef (_, id, _, _) as f ->
        let id = update_name_module in_mod id in
        (Scope.set env id f, f)
        (* Return the function definition statement *)
    | Match (loc, pattern, cases) ->
        let env, stmt = eval_match env loc (env, pattern) cases ~in_mod in
        (* clean up local variables used inside the match *)
        (Scope.pop_scope env, stmt)
    | If (loc, cond, t, f) -> (
        let n_env, eval_cond = eval_expr env ~in_mod loc (env, cond) in
        match eval_cond with
        | Terminal (Const (V_Boolean b)) ->
            if b then eval_statement n_env ~in_mod t
            else eval_statement n_env ~in_mod f
        | _ ->
            raise (Located_error (`Wrong_Type (T_Boolean, T_Auto), loc)) )
    | ModuleDef (loc, n, stmts) ->
        (* we can add this module into the current environnement *)
        let n_env = Scope.add_module env n in
        let mod_name = Some n in
        let rec aux menv = function
          | [] ->
              (menv, Expression (loc, Terminal (Const V_Void), T_Void))
          | stmt :: t ->
              Printf.printf "Inside module: %s\n"
                (Option.value ~default:"None" mod_name) ;
              let nenv, _ = eval_statement menv ~in_mod:mod_name stmt in
              aux nenv t (* Otherwise, continue with the next statement *)
        in
        aux n_env stmts
  (* for the moment we're not getting the type of the expression *)

  let exec (p : program) =
    let _env = Scope.create () in
    let rec aux env = function
      | [] ->
          ()
      | stmt :: t ->
          let nenv, _stmt = eval_statement env stmt in
          aux nenv t
    in
    Located_error.handle_type_exception aux _env p;
end
