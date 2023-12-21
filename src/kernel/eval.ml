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
          let str = Printf.sprintf "Variable %s definition error." ident in
          raise (Located_error (`Language_Error str, loc)) )

  let rec binop_helper env loc v v' =
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
        aux k k'
    | Terminal (Const k), Terminal (V_Var (id, _)) ->
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
          aux k value
        else raise (Located_error (`Not_Number, loc))
    | Terminal (V_Var (id, _)), Terminal (Const k) ->
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
    | Terminal (V_Var (id, _)), Terminal (V_Var (id', _)) ->
        let same_type = get_type env loc id = get_type env loc id' in
        if same_type then
          let v = get_value env loc id in
          let v' = get_value env loc id' in
          match (v, v') with
          | `Expression (Terminal (Const k)), `Expression (Terminal (Const k'))
            ->
              aux k k'
          | _ ->
              raise (Located_error (`Not_Number, loc))
        else raise (Located_error (`Not_Number, loc))
    | a, b ->
        let tmp, v_a = eval_expr env loc (env, a) in
        let tmp', v_b = eval_expr tmp loc (tmp, b) in
        binop_helper tmp' loc v_a v_b

  and bincomp_helper env loc v v' =
    let aux a b = function
      | Equal ->
          Boolean (a = b)
      | NotEqual ->
          Boolean (a <> b)
      | GEQ ->
          Boolean (a >= b)
      | LEQ ->
          Boolean (a <= b)
      | Greater ->
          Boolean (a > b)
      | Lesser ->
          Boolean (a < b)
    in
    match (v, v') with
    | Number k, Number k' ->
        aux k k'
    | String k, String k' ->
        aux k k'
    | Boolean k, Boolean k' ->
        aux k k'
    | (Variable (id, _) as v), ((Number _ | String _ | Boolean _) as e)
    | ((Number _ | String _ | Boolean _) as e), (Variable (id, _) as v) ->
        let e_typ = val_to_typ e in
        let same_typ = is_variable_type env loc id e_typ in
        if same_typ then
          let v_val = get_value env loc id in
          match v_val with
          | `Expression v_val ->
              aux v_val e
          | _ ->
              raise (Located_error (`Function_Value, loc))
        else raise (Located_error (`Wrong_Type (val_to_typ v, e_typ), loc))
    | a, b ->
        let tmp, v_a = eval_expr env loc (env, a) in
        let tmp', v_b = eval_expr tmp loc (tmp, b) in
        bincomp_helper tmp' loc v_a v_b

  (* compute_list recreates a list object from a syntax like h :: t. [env] is
     the current environement in which we're doing this [loc] is the location of
     the statement that asked to create that list [head] is the head of the list
     we want to create (the h in h :: t) [tail] is an identificator for the list
     tail we want to create *)
  and compute_list env loc head (tail : identificator) =
    let v = get_value env loc tail in
    match v with
    | `Expression (List (h, t)) -> (
      match h with None -> List (head, t) | Some _ -> List (head, List (h, t)) )
    | _ ->
        raise (Located_error (`Wrong_Type (T_List, T_Auto), loc))

  (* eval_function evaluates a function call inside the AST to remplace it by
     the actual value returned (or not) by the function. this function processes
     the given arguments first, then populates a new scope made out of the
     current one but with the new local variables defined in the function
     definition *)
  and eval_function env loc id expr_list =
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
          process_args (eval_expr env loc h :: acc) t
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
      | (id, _) :: t, (_, h) :: t' ->
          add_to_scope (Scope.set acc id (Expression (loc, h, T_Auto))) t t'
    in
    let f_env = add_to_scope (Scope.push_scope env) params processed_args in
    (* now we can evaluate the function statements *)
    let rec aux env = function
      | [] ->
          raise (Located_error (`Expected_Return_Statement, loc))
      | stmt :: t -> (
          let n_env, stmt = eval_statement env stmt in
          match stmt with
          | Return (_, e) ->
              let expected_ret =
                Scope.get env id
                |> function
                | Some (FuncDef (_, (_, t), _, _)) ->
                    t
                | _ ->
                    raise (Located_error (`Not_A_Callable, loc))
              in
              let scp, processed = eval_expr n_env loc (n_env, e) in
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

  and eval_expr _env loc = function
    | env, Terminal (Const V_Void) ->
        (env, Terminal (Const V_Void))
    | (_, Terminal (Const (V_Number _ | V_String _ | V_Boolean _ | V_List _)))
      as v ->
        v
    | env, Terminal (V_Var (id, _)) -> (
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
        let env', a' = eval_expr env loc (env, a) in
        let env'', b' = eval_expr env' loc (env', b) in
        match op with
        | `Compare bincomp ->
            (env'', bincomp_helper env loc a' b' bincomp)
        | `Operator binop ->
            (env'', binop_helper env loc a b binop) )
    | env, FuncCall (id, expr_list) ->
        let rec exprs acc = function
          | [] ->
              List.rev acc
          | h :: t ->
              exprs ((env, h) :: acc) t
        in
        eval_function env loc id (exprs [] expr_list)

  and eval_match env loc pattern cases =
    let n_env, eval_pattern = eval_expr env loc pattern in
    let rec iterate_cases = function
      | [] ->
          (n_env, Expression (loc, Empty, T_Void))
      (* if we find a wildcard then we can stop analysis of the match
         statement *)
      | (Empty, stmts) :: _ ->
          eval_statement n_env (Block (loc, stmts))
      (* same if we find a corresponding pattern *)
      | ( List
            (Some ((Number _ | String _ | Boolean _) as term), Variable (id, _))
        , stmts )
        :: _ -> (
          if not (val_to_typ eval_pattern = T_List) then
            iterate_cases (List.tl cases)
          else
            let hd = get_list_head loc eval_pattern in
            match hd with
            | Some k ->
                if compare_expr k term then
                  let n_env =
                    Scope.set n_env id
                      (Expression
                         ( loc
                         , get_list_tail loc eval_pattern
                         , val_to_typ eval_pattern ) )
                  in
                  eval_statement n_env (Block (loc, stmts))
                else iterate_cases (List.tl cases)
            | None ->
                iterate_cases (List.tl cases) )
      | (List (Some (Variable (id, _)), Variable (id', _)), stmts) :: _ -> (
          if not (val_to_typ eval_pattern = T_List) then
            iterate_cases (List.tl cases)
          else
            let hd = get_list_head loc eval_pattern in
            let tail = get_list_tail loc eval_pattern in
            match hd with
            | Some k ->
                let tmp = Scope.push_scope n_env in
                let tmp' =
                  Scope.set tmp id (Expression (loc, k, val_to_typ eval_pattern))
                in
                let n_env =
                  Scope.set tmp' id'
                    (Expression (loc, tail, val_to_typ eval_pattern))
                in
                eval_statement n_env (Block (loc, stmts))
            | None ->
                iterate_cases (List.tl cases) )
      | (case, stmts) :: _ ->
          let n_env, eval_case = eval_expr n_env loc (n_env, case) in
          if compare_expr eval_pattern eval_case then
            eval_statement n_env (Block (loc, stmts))
          else iterate_cases (List.tl cases)
    in
    iterate_cases cases

  and eval_statement env = function
    | Return (loc, e) ->
        Utils.Logger.Logger.debug "Return statement found" ;
        (env, Return (loc, e)) (* Wrap the expression in a Return statement *)
    | Expression (loc, e, _typ) ->
        let env, expr = eval_expr env loc (env, e) in
        (env, Expression (loc, expr, _typ))
        (* Wrap the expression in an Expression statement *)
    | Block (loc, stmts) ->
        let blck_env = Scope.push_scope env in
        let rec aux env = function
          | [] ->
              ( Scope.pop_scope env
              , Expression (loc, Terminal (Const V_Void), T_Void) )
          | stmt :: t -> (
              let nenv, stmt = eval_statement env stmt in
              match stmt with
              | Return _ ->
                  (nenv, stmt)
                  (* If the statement is a Return statement, immediately return
                     it *)
              | _ ->
                  aux nenv t (* Otherwise, continue with the next statement *) )
        in
        aux blck_env stmts
    | Assign (loc, (id, _t), e) ->
        let tmp, processed = eval_expr env loc (env, e) in
        if _t <> T_Auto && _t <> val_to_typ processed then
          raise (Located_error (`Wrong_Type (_t, val_to_typ processed), loc))
        else
          let n_env =
            Scope.set tmp id (Expression (loc, processed, val_to_typ processed))
          in
          (n_env, Expression (loc, Terminal (Const V_Void), T_Void))
    | FuncDef (_, (id, _), _, _) as f ->
        (Scope.set env id f, f) (* Return the function definition statement *)
    | Match (loc, pattern, cases) ->
        let env, stmt = eval_match env loc (env, pattern) cases in
        (env, stmt)
        (* Return the match statement *)
    | If (loc, cond, t, f) -> (
        let n_env, eval_cond = eval_expr env loc (env, cond) in
        match eval_cond with
        | Terminal (Const (V_Boolean b)) ->
            if b then (
              Utils.Logger.Logger.debug "True statement found" ;
              eval_statement n_env t )
            else eval_statement n_env f
        | _ ->
            raise (Located_error (`Wrong_Type (T_Boolean, T_Auto), loc)) )
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
    aux _env p
end
