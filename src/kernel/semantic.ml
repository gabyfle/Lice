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

module Typing = struct
  exception Language_Error of location * string

  exception Not_Integer

  exception Not_Number

  exception Undefined_Function

  exception Not_A_Callable

  exception Wrong_Parameters_Number of int * int

  exception Wrong_Parameter_Type of identificator * typ * typ

  let func_call_type_check expected params =
    if List.length expected <> List.length params then
      raise (Wrong_Parameters_Number (List.length expected, List.length params))
    else
      List.iter2
        (fun (ty, id) (v, _) ->
          if ty <> v then raise (Wrong_Parameter_Type (id, ty, v)))
        expected params

  let is_callable (env: Scope.t) (ident: string): bool =
    match Scope.lookup env ident with
    | Some (FuncDef _) -> true
    | Some _ -> false
    | None -> false
  
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
          raise Not_Number
      | FuncCall (ident, params) -> (
          let t = Scope.get env ident in
          match t with
          | Some func -> match func with
            | FuncDef(_, _, _, _) -> ()
            | _ -> raise Not_A_Callable
          | None ->
              raise Undefined_Function )
      | List lexpr ->
          let rec aux acc = function
            | [] ->
                acc
            | h :: t ->
                let v = expr_type_check env h in
                if v <> T_Auto then failwith "none"
          in
          aux T_Auto lexpr )
end
