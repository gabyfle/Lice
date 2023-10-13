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

open Types
open Value

exception Language_Error of location * string

exception Not_Integer

exception Not_Number

module Converter = struct
  let type_to_value = function
    | T_Boolean ->
        V_Boolean None
    | T_String ->
        V_String None
    | T_List ->
        V_List None
    | T_Number ->
        V_Number None
    | T_Void ->
        V_Void
    | T_Auto ->
        V_Void (* for the moment it's void, subject to changes *)

  let rec expr_to_value = function
    | Empty ->
        V_Void
    | Number n ->
        V_Number (Some n)
    | String s ->
        V_String (Some s)
    | Boolean b ->
        V_Boolean (Some b)
    | Variable (id, t) ->
        V_Variable (id, type_to_value t)
    | BinOp (bin, a, b) -> (
        let a_val = expr_to_value a in
        let b_val = expr_to_value b in
        (* Perform the operation or return a default value if a conversion fails *)
        match (a_val, b_val) with
        | V_Number (Some a), V_Number (Some b) -> (
          match bin with
          | Plus ->
              V_Number (Some (a +. b))
          | Minus ->
              V_Number (Some (a -. b))
          | Multiply ->
              V_Number (Some (a *. b))
          | Divide ->
              if b = 0. then raise Division_by_zero else V_Number (Some (a /. b))
          | Mod ->
              (* this part will surely need to be rewrited as we're casting maybe to many times *)
              let is_integer x = float_of_int (int_of_float x) = x in
              if not (is_integer b) then raise Not_Integer
              else if b = 0. then raise Division_by_zero
              else
                let int_a, int_b = (int_of_float a, int_of_float b) in
                V_Number (Some (float_of_int (int_a mod int_b))) )
        | _ ->
            raise Not_Number )
    | FuncCall (_, _) ->
        V_Void
    | List lexpr ->
        let rec aux acc = function
          | [] ->
              V_List (Some (List.rev acc))
          | h :: t ->
              let v = expr_to_value h in
              aux (v :: acc) t
        in
        aux [] lexpr

  let rec statement_to_values = function
    | Return e -> Return (expr_to_value e)
    | Expression (loc, e) -> Expression(loc, (expr_to_value e))
    | Block (loc, stmts) ->
        let rec aux acc = function
            | [] -> acc
            | h :: t -> aux ((statement_to_values h) :: acc) t 
        in
        Block (loc, (aux [] stmts))
    | Assign (t, e) -> Assign(t, (expr_to_value e))
    | FuncDef (loc, name, params, body) ->
    | Match (loc,e, cases) ->

  let rec program_to_values =
    let rec aux acc = function
      | [] ->
          acc
      | h :: t ->
          aux (statement_to_values h :: acc) t
    in
    aux []
end
