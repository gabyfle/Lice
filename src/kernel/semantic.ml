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
open Utils
open Utils.Logger

module type TYPING = sig
  type exception_type =
    [ `Language_Error of string
    | `Not_Integer
    | `Not_Number
    | `Division_by_zero
    | `Undefined_Function
    | `Not_A_Callable
    | `Wrong_Parameters_Number of int * int
    | `Wrong_Parameter_Type of identificator * typ * typ
    | `Wrong_Assign_Type of identificator * typ * typ
    | `Wrong_Return_Type of identificator * typ * typ
    | `Wrong_Case_Type of typ * typ ]

  exception Located_error of exception_type * location

  val type_check : program -> unit
end

module Typing : TYPING = struct
  type exception_type =
    [ `Language_Error of string
    | `Not_Integer
    | `Not_Number
    | `Division_by_zero
    | `Undefined_Function
    | `Not_A_Callable
    | `Wrong_Parameters_Number of int * int
    | `Wrong_Parameter_Type of identificator * typ * typ
    | `Wrong_Assign_Type of identificator * typ * typ
    | `Wrong_Return_Type of identificator * typ * typ
    | `Wrong_Case_Type of typ * typ ]

  exception Located_error of exception_type * location

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

  let func_call_type_check expected params loc =
    if List.length expected <> List.length params then
      raise
        (Located_error
           ( `Wrong_Parameters_Number (List.length expected, List.length params)
           , loc ) )
    else
      List.iter2
        (fun (id, ty) (_, v) ->
          if ty <> v then
            raise (Located_error (`Wrong_Parameter_Type (id, ty, v), loc)) )
        expected params

  let is_callable (env : Scope.t) (ident : string) :
      (typed_ident * typed_ident list) option =
    match Scope.get env ident with
    | Some (FuncDef (_, tid, params, _)) ->
        Some (tid, params)
    | _ ->
        None

  let expr_type_check env (loc : location) = function
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
    | BinOp (bin, a, b) -> (
      (* Perform the operation or return a default value if a conversion fails *)
      match (a, b) with
      | Number _, Number v' -> (
        match bin with
        | Plus ->
            T_Number
        | Minus ->
            T_Number
        | Multiply ->
            T_Number
        | Divide ->
            if v' = 0. then raise (Located_error (`Division_by_zero, loc))
            else T_Number
        | Mod ->
            (* this part will surely need to be rewrited as we're casting maybe to many times *)
            let is_integer x = float_of_int (int_of_float x) = x in
            if not (is_integer v') then raise (Located_error (`Not_Integer, loc))
            else if v' = 0. then raise (Located_error (`Division_by_zero, loc))
            else T_Number )
      | Variable (_, T_Number), Number _ | Number _, Variable (_, T_Number) ->
          T_Number
      | Variable (id, T_Auto), Number _ | Number _, Variable (id, T_Auto) ->
          let is_number = is_variable_type env id T_Number in
          if is_number then T_Number
          else raise (Located_error (`Not_Number, loc))
      | _ ->
          raise (Located_error (`Not_Number, loc)) )
    | FuncCall (ident, params) -> (
      match is_callable env ident with
      | Some (tid, expected) ->
          let _, t = tid in
          func_call_type_check expected params loc ;
          t
      | None ->
          raise (Located_error (`Not_A_Callable, loc)) )
    | List _ ->
        T_List

  let assign_type_check env id expected expression loc =
    match Scope.get env id with
    | Some (Expression (_, _, t)) ->
        let t' = expr_type_check env loc expression in
        if t' <> t then
          raise (Located_error (`Wrong_Assign_Type (id, t, t'), loc))
        else (
          Logger.debug "Pushed variable %s type %s" id (typ_to_string t') ;
          Scope.set env id (Expression (loc, Empty, t')) ) ;
        T_Void
    | None ->
        let t = expr_type_check env loc expression in
        if t = expected then (
          Logger.debug "Pushed variable %s type %s" id (typ_to_string t) ;
          Scope.set env id (Expression (loc, Empty, t)) ;
          T_Void )
        else raise (Located_error (`Wrong_Assign_Type (id, expected, t), loc))
    | _ ->
        raise
          (Located_error
             ( `Language_Error
                 "An error occured while trying to assign a variable"
             , loc ) )

  let rec funcdef_type_check env (fname : identificator) (expected : typ) params
      stmts loc =
    (* we first need to create all the needed variables inside our function scope *)
    let scope = Scope.push_scope env in
    List.iter
      (fun (id, ty) -> Scope.set scope id (Expression (loc, Empty, ty)))
      params ;
    let get_return_type ret_loc expression =
      let ret_typ = expr_type_check scope ret_loc expression in
      ret_typ
    in
    let rec perform_check_stmts = function
      | [] ->
          Scope.set env fname
            (FuncDef
               ( loc
               , (fname, expected)
               , params
               , Expression (loc, Empty, expected) ) ) ;
          T_Void
      | Return (loc', e) :: t ->
          let ret_type = get_return_type loc' e in
          if ret_type <> expected then
            raise
              (Located_error
                 (`Wrong_Return_Type (fname, expected, ret_type), loc) )
          else perform_check_stmts t
      | h :: t ->
          ignore (stmt_type_check scope h) ;
          perform_check_stmts t
    in
    perform_check_stmts stmts

  and match_type_check env match_expr (cases : (expr * statement) list) loc =
    let mtyp = expr_type_check env loc match_expr in
    (* the type of the match *)
    let check_cases ((e : expr), (stmt : statement)) =
      let ctyp = expr_type_check env loc e in
      if ctyp <> mtyp then
        raise (Located_error (`Wrong_Case_Type (mtyp, ctyp), loc))
      else ignore (stmt_type_check env stmt)
    in
    List.iter check_cases cases

  and stmt_type_check env stmt =
    match stmt with
    | Return (loc, e) ->
        expr_type_check env loc e
    | Expression (loc, e, _) ->
        expr_type_check env loc e
    | Block (_, stmts) ->
        let rec aux = function
          | [] ->
              ()
          | h :: t ->
              ignore (stmt_type_check env h) ;
              aux t
        in
        aux stmts ; T_Void
    | Assign (loc, (id, t), e) ->
        assign_type_check env id t e loc
    | FuncDef (loc, (id, ret_type), params, Block (_, stmts)) ->
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
    | Located_error (`Wrong_Parameters_Number (a, b), loc) ->
        let str = Formatting.params_number_error loc a b in
        Logger.error "%s" str ; exit 1
    | Located_error (`Wrong_Parameter_Type (_, a, b), loc)
    | Located_error (`Wrong_Assign_Type (_, a, b), loc)
    | Located_error (`Wrong_Return_Type (_, a, b), loc)
    | Located_error (`Wrong_Case_Type (a, b), loc) ->
        let ta = typ_to_string a in
        let tb = typ_to_string b in
        let str = Formatting.typing_error loc ta tb in
        Logger.error "%s" str ; exit 1

  let type_check (p : program) =
    let global_scope = Scope.create () in
    let rec aux = function
      | [] ->
          ()
      | stmt :: t ->
          ignore (handle_type_exception stmt_type_check global_scope stmt) ;
          aux t
    in
    aux p
end
