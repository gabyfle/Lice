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

module type EVAL = sig
  val exec : program -> unit

  val exec_string : string -> unit
end

module Eval : EVAL = struct
  let get_value env loc ident =
    match Scope.get env ident with
    | None ->
        raise (Located_error (`Undefined_Variable ident, loc))
    | Some h -> (
      match h with
      | (Expression (_, _, _) | FuncDef (_, _, _, _)) as e ->
          e
      | _ ->
          let str = Printf.sprintf "Variable %s definition error." ident in
          raise (Located_error (`Language_Error str, loc)) )

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

  let bincomp_helper v v' = function
    | Equal ->
        Boolean (v = v')
    | NotEqual ->
        Boolean (v <> v')
    | GEQ ->
        Boolean (v >= v')
    | LEQ ->
        Boolean (v <= v')
    | Greater ->
        Boolean (v > v')
    | Lesser ->
        Boolean (v < v')

  (* compute_list recreates a list object from a syntax like h :: t. [env] is
     the current environement in which we're doing this [loc] is the location of
     the statement that asked to create that list [head] is the head of the list
     we want to create (the h in h :: t) [tail] is an identificator for the list
     tail we want to create *)
  let compute_list env loc head (tail : identificator) =
    let v = get_value env loc tail in
    match v with
    | Expression (_, List (h, t), _) -> (
      match h with None -> List (head, t) | Some _ -> List (head, List (h, t)) )
    | _ ->
        raise (Located_error (`Wrong_Type (T_List, T_Auto), loc))

  let eval_expr env loc = function
    | Empty ->
        ()
    | Number _ | String _ | Boolean _ ->
        ()
    | List (h, t) ->
        ()
    | Variable name ->
        ()
    | BinOp (_, _, _) ->
        ()
    | FuncCall (_, _) ->
        ()

  let exec (_ : program) = ()

  let exec_string (_ : string) = ()
end
