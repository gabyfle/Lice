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

open Ast.Types
open Env
open Located_error
open Utils
open Utils.Logger

module type TYPING = sig
  val is_variable_type : Scope.t -> identificator -> typ -> bool

  val type_check : program -> Scope.t
end

module Typing : TYPING = struct
  let expected_return_typ = ref ("", T_Auto)

  let is_variable_type env id t =
    match Scope.get env id with
    | Some (Expression (_, Empty, t')) ->
        t = t'
    | _ ->
        false

  let get_variable_type env id =
    match Scope.get env id with
    | Some (Expression (_, Empty, t)) ->
        t
    | _ ->
        T_Auto

  let is_callable (env : Scope.t) (ident : string) :
      (typed_ident * typed_ident list) option =
    match Scope.get env ident with
    | Some (FuncDef (_, tid, params, _)) ->
        Some (tid, params)
    | _ ->
        None

  (* binop_type_check [env] [loc] [a] [b] [op] performs a type check on a binary
     operation involving two operands

     [env] is the current environement, [loc] is the location of the binary
     operation, [a] is the first operand (left) [b] is the second operand
     (right) and [op] is the operator involved in the operation *)
  let rec binop_type_check env (loc : location) a b op =
    match (a, b) with
    | Number _, Number v' -> (
      match op with
      | Plus | Minus | Multiply ->
          T_Number
      | Divide ->
          if v' = 0. then raise (Located_error (`Division_by_zero, loc))
          else T_Number
      | Mod ->
          if not (Float.is_integer v') then
            raise (Located_error (`Not_Integer, loc))
          else if v' = 0. then raise (Located_error (`Division_by_zero, loc))
          else T_Number )
    | Variable (_, T_Number), Number _
    | Number _, Variable (_, T_Number)
    | Variable (_, T_Number), Variable (_, T_Number) ->
        T_Number
    | Variable (id, T_Auto), Number v | Number v, Variable (id, T_Auto) ->
        let is_number = is_variable_type env id T_Number in
        if is_number then binop_type_check env loc (Number 1.) (Number v) op
        else raise (Located_error (`Not_Number, loc))
    | Variable (id, t), Variable (id', t') ->
        let typ_v1 = expr_type_check env loc (Variable (id, t)) in
        let typ_v2 = expr_type_check env loc (Variable (id', t')) in
        if typ_v1 <> T_Number && typ_v2 <> T_Number then
          raise (Located_error (`Not_Number, loc))
        else T_Number
    | FuncCall (id, params), Number v | Number v, FuncCall (id, params) ->
        let func_ret_type = expr_type_check env loc (FuncCall (id, params)) in
        if func_ret_type <> T_Number then
          raise (Located_error (`Not_Number, loc))
        else binop_type_check env loc (Number 1.) (Number v) op
    | _ ->
        raise (Located_error (`Not_Number, loc))

  (* expr_type [env] [loc] [expr] performs a type check on the given expression

     [env] is the current environement on which we're doing the check [loc] is
     the location of the expression we're currently checking [expr] is the
     actual expression to check

     this function is recursive. in the near future, if we have performances
     problems, we might check back this function to improve performaces as i'm
     pretty sure every troubles will come from this file :p *)
  and expr_type_check env (loc : location) = function
    | Empty ->
        T_Void
    | Number _ ->
        T_Number
    | String _ ->
        T_String
    | Boolean _ ->
        T_Boolean
    | Variable (id, t) ->
        if t = T_Auto then get_variable_type env id else t
    | BinOp (`Compare _, _, _) ->
        T_Boolean
    | BinOp (`Operator op, a, b) ->
        binop_type_check env loc a b op
    | FuncCall (ident, params) -> (
      match is_callable env ident with
      | Some (tid, expected) ->
          let _, t = tid in
          func_call_type_check env ident expected params loc ;
          t
      | None ->
          raise (Located_error (`Not_A_Callable, loc)) )
    | List _ ->
        T_List

  (* func_call_type_check [env] [fname: string] [expected] [params] performs a
     type check on a function call. it does checks that all given parameters do
     have the correct type when calling the given function

     [env] is the current scope on which we're doing the check [fname] is the
     function name, used to retreive function's properties into the environement
     [expected] are the actual expected parameters types [params] is a list of
     expressions given as parameters when doing the function call *)
  and func_call_type_check (env : Scope.t) fname expected (params : expr list)
      loc =
    if List.length expected <> List.length params then
      raise
        (Located_error
           ( `Wrong_Parameters_Number
               (fname, List.length expected, List.length params)
           , loc ) )
    else
      List.iter2
        (fun (id, ty) e ->
          let ptyp = expr_type_check env loc e in
          if ty <> ptyp then
            raise (Located_error (`Wrong_Parameter_Type (id, ty, ptyp), loc)) )
        expected params

  (* assign_type_check [env] [id] [expected] [expression] [loc] performs a type
     check on an assign statement. it does check that the given expression has
     the correct type expected by the variable definition

     [env] is the current environement we're doing the assign in, [id] is the
     name of the variable we're creating and/or assigning. [expected] is the
     expected type, given during the assign [expression] is the expression we're
     trying to assign to the variable and [loc] is the location of the
     statement *)
  let assign_type_check env id expected expression loc =
    match Scope.get env id with
    | Some (Expression (_, _, t)) ->
        let t' = expr_type_check env loc expression in
        if t' <> t then
          raise (Located_error (`Wrong_Assign_Type (id, t, t'), loc))
        else Scope.set env id (Expression (loc, Empty, t')) ;
        T_Void
    | None ->
        let t = expr_type_check env loc expression in
        if t = expected then (
          Scope.set env id (Expression (loc, Empty, t)) ;
          T_Void )
        else raise (Located_error (`Wrong_Assign_Type (id, expected, t), loc))
    | _ ->
        raise
          (Located_error
             ( `Language_Error
                 "An error occured while trying to assign a variable"
             , loc ) )

  (* add_func_to_scope [env] [fname] [expected] [params] [loc] adds a new
     function to the scope given in parameter.

     [env] is the actual scope [fname] is the name of the function to add
     [expected] is the expected return type of the function [params] are the
     expected params of the function and [loc] is the location of the function
     definition *)
  let add_func_to_scope env (fname : identificator) (expected : typ) params loc
      =
    Scope.set env fname
      (FuncDef
         (loc, (fname, expected), params, Expression (loc, Empty, expected)) )

  (* funcdef_type_check [env] [fname] [expected] [params] performs a type check
     on the defined function [fname]. Same parameters than add_func_to_scope
     excepted for [stmts] which is the list of the statements of the function *)
  let rec funcdef_type_check env (fname : identificator) (expected : typ) params
      stmts loc =
    (* we first need to create all the needed variables inside our function
       scope *)
    let scope = Scope.push_scope env in
    List.iter
      (fun (id, ty) -> Scope.set scope id (Expression (loc, Empty, ty)))
      params ;
    (* to allow recursivity, we add it to the local scope of the function *)
    add_func_to_scope scope fname expected params loc ;
    let rec perform_check_stmts = function
      | [] ->
          Logger.debug "Leaving function: %s" fname ;
          expected_return_typ := ("", T_Auto) ;
          add_func_to_scope env fname expected params loc ;
          T_Void
      | h :: t ->
          expected_return_typ := (fname, expected) ;
          ignore (stmt_type_check scope h) ;
          perform_check_stmts t
    in
    perform_check_stmts stmts

  (* match_type_check [env] [match_expr] [cases] [loc] performs a type checking
     onto a matching expression.

     [env] is the actual scope [match_expr] is the expression we're trying to
     match [cases] is the list of case matches to match [match_expr] and [loc]
     is the location of the matching expr *)
  and match_type_check env match_expr (cases : (expr * statement list) list) loc
      =
    let rec ignore_iter f = function
      | [] ->
          ()
      | h :: t ->
          ignore (f h) ;
          ignore_iter f t
    in
    let mtyp = expr_type_check env loc match_expr in
    (* the type of the match *)
    let check_cases (e, stmts) =
      match (e, stmts) with
      | List (_, Variable (ident, T_Auto)), _ ->
          let scope = Scope.push_scope env in
          Scope.set scope ident (Expression (loc, Empty, T_List)) ;
          ignore_iter (stmt_type_check scope) stmts
      | _ ->
          let ctyp = expr_type_check env loc e in
          if ctyp <> mtyp then
            raise (Located_error (`Wrong_Case_Type (mtyp, ctyp), loc))
          else ignore_iter (stmt_type_check env) stmts
    in
    List.iter check_cases cases

  (* stmt_type_check [env] [stmt] performs a type check on a whole statement.

     [env] is the environement on which we perform the type check of the
     statement [stmt] is the statement to check *)
  and stmt_type_check env stmt =
    Logger.debug "STMT CHECK: Statement encountred: %s" (stmt_to_string stmt) ;
    match stmt with
    | Return (loc, e) ->
        let expr_typ = expr_type_check env loc e in
        let fname, expected = !expected_return_typ in
        if expected <> T_Auto && expected = expr_typ then expr_typ
        else
          raise
            (Located_error (`Wrong_Return_Type (fname, expected, expr_typ), loc))
    | Expression (loc, e, _) ->
        expr_type_check env loc e
    | Block (_, stmts) ->
        let scope = Scope.push_scope env in
        (* a block has its own scope *)
        let rec aux = function
          | [] ->
              ()
          | h :: t ->
              ignore (stmt_type_check scope h) ;
              aux t
        in
        aux stmts ; T_Void
    | Assign (loc, (id, t), e) ->
        assign_type_check env id t e loc
    | FuncDef (loc, (id, ret_type), params, Block (_, stmts)) ->
        Logger.debug "Entering function: %s" id ;
        funcdef_type_check env id ret_type params stmts loc
    | FuncDef (loc, _, _, _) ->
        raise
          (Located_error
             ( `Language_Error
                 "An error occured while trying to type check a function"
             , loc ) )
    | Match (loc, expression, cases) ->
        match_type_check env expression cases loc ;
        T_Void
    | If (loc, cond, pass, break) ->
        (* pass: stmts when the cond is true; break: stmts when the cond is
           false *)
        let cond_typ = expr_type_check env loc cond in
        if cond_typ <> T_Boolean then
          raise (Located_error (`Wrong_Case_Type (T_Boolean, cond_typ), loc))
        else (
          ignore (stmt_type_check env pass) ;
          ignore (stmt_type_check env break) ;
          T_Void )

  (* handle_type_exception is an helper function that allows to display nice
     error messages and to handle Located_error exception during the type
     checking process. *)
  let handle_type_exception f a b =
    try f a b with
    | Located_error (`Language_Error s, loc) ->
        let str = Formatting.misc_error loc s in
        Logger.error "%s" str ; exit 1
    | Located_error (`Not_Integer, loc) ->
        let str =
          Formatting.misc_error loc
            "Attempting to do modulo arithmetics with a non-integer"
        in
        Logger.error "%s" str ; exit 1
    | Located_error (`Not_Number, loc) ->
        let str =
          Formatting.misc_error loc
            "Attempting to do number operation with a non-number"
        in
        Logger.error "%s" str ; exit 1
    | Located_error (`Division_by_zero, loc) ->
        let str =
          Formatting.misc_error loc "Division by zero occurred. Aborting"
        in
        Logger.error "%s" str ; exit 1
    | Located_error (`Undefined_Function, loc) ->
        let str = Formatting.misc_error loc "Calling an undefined function" in
        Logger.error "%s" str ; exit 1
    | Located_error (`Not_A_Callable, loc) ->
        let str =
          Formatting.misc_error loc "Trying to call a non callable object"
        in
        Logger.error "%s" str ; exit 1
    | Located_error (`Wrong_Parameters_Number (fname, a, b), loc) ->
        let str = Formatting.params_number_error loc fname a b in
        Logger.error "%s" str ; exit 1
    | Located_error (`Wrong_Parameter_Type (_, a, b), loc)
    | Located_error (`Wrong_Assign_Type (_, a, b), loc)
    | Located_error (`Wrong_Return_Type (_, a, b), loc)
    | Located_error (`Wrong_Case_Type (a, b), loc)
    | Located_error (`Wrong_Type (a, b), loc) ->
        let ta = typ_to_string a in
        let tb = typ_to_string b in
        let str = Formatting.typing_error loc ta tb in
        Logger.error "%s" str ; exit 1

  (* type_check [p] is the main entry of the type checking module. it performs a
     type check using all the above functions on the statement list given in
     parameter

     [p] is the program to type check (which is, a statement list) *)
  let type_check (p : program) =
    let global_scope = Scope.create () in
    let rec aux = function
      | [] ->
          global_scope
      | stmt :: t ->
          Logger.debug "TYPE CHECK: Statement encountred: %s"
            (stmt_to_string stmt) ;
          ignore (handle_type_exception stmt_type_check global_scope stmt) ;
          aux t
    in
    aux p
end
