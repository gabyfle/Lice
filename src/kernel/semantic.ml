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
open Utils.Logger

module Typing = struct
  exception Language_Error of string

  exception Not_Integer

  exception Not_Number

  exception Undefined_Function

  exception Not_A_Callable

  exception Wrong_Parameters_Number of int * int

  exception Wrong_Paramteter_Type of identificator * typ * typ

  exception Wrong_Assign_Type of identificator * typ * typ

  let func_call_type_check expected params =
    if List.length expected <> List.length params then
      raise (Wrong_Parameters_Number (List.length expected, List.length params))
    else
      List.iter2
        (fun (id, ty) (_, v) ->
          if ty <> v then raise (Wrong_Parameter_Type (id, ty, v)) )
        expected params

  let is_callable (env : Scope.t) (ident : string) :
      (typed_ident * typed_ident list) option =
    match Scope.get env ident with
    | Some (FuncDef (_, tid, params, _)) ->
        Some (tid, params)
    | _ ->
        None

  let rec expr_type_check env = function
    | Empty ->
        T_Void
    | Number _ ->
        T_Number
    | String _ ->
        T_String
    | Boolean _ ->
        T_Boolean
    | Variable (_, t) ->
        t
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
            if v' = 0. then raise Division_by_zero else T_Number
        | Mod ->
            (* this part will surely need to be rewrited as we're casting maybe to many times *)
            let is_integer x = float_of_int (int_of_float x) = x in
            if not (is_integer v') then raise Not_Integer
            else if v' = 0. then raise Division_by_zero
            else T_Number )
      | _ ->
          raise Not_Number )
    | FuncCall (ident, params) -> (
      match is_callable env ident with
      | Some (tid, expected) ->
          let _, t = tid in
          func_call_type_check expected params ;
          t
      | None ->
          raise Not_A_Callable )
    | List _ ->
        T_List

  let rec assign_type_check env id expected expression loc =
    match Scope.get env id with
    | Some (Expression (_, _, t)) ->
        let t' = expr_type_check env expression in
        if t' <> t then raise (Wrong_Assign_Type (id, t, t')) else T_Void
    | None ->
        let t = expr_type_check env expression in
        if t = expected then (
          Scope.set env id (Expression (loc, expression, t)) ;
          T_Void )
        else t
    | _ ->
        raise
          (Language_Error "An error occured while trying to assign a variable")

  let rec stmt_type_check env stmt =
    match stmt with
    | Return e ->
        expr_type_check env e
    | Expression (loc, e, t_) ->
        expr_type_check env e
    | Block (loc, stmts) ->
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
    | _ ->
        T_Auto
end
