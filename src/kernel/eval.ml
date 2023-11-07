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

module type EVAL = sig
  val exec : Scope.t -> program -> unit
end

module Eval : EVAL = struct
  let binop_helper v v' = function
  | Plus ->
      Number (v +. v')
  | Minus ->
      Number (v -. v')
  | Multiply ->
      Number (v *. v')
  | Divide ->
      if v' = 0. then raise Division_by_zero else Number (v /. v')
  | Mod ->
      if not (Float.is_integer v') then raise Division_by_zero
      else if v' = 0. then raise Division_by_zero
      else
        let int_v = Int.of_float v in
        let int_v' = Int.of_float v' in
        Number (Float.of_int (int_v mod int_v'))

  let rec binop_eval env a b op =
    match (a, b) with
    | Number v, Number v' -> binop_helper v v' op
    | Variable (v, T_Number), Number v' ->
      let a = Scope.get env v in
      match a with
        | None -> failwith "Undefined variable"
        | Some stmt ->

  let exec (scope : Scope.t) (prog : program) = ()
end
