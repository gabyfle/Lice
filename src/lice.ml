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
open Kernel
open Utils.Logger

let () =
  let code = "let x = h; \n  \n  let f = x + 12;\n\n  \n  let f = a * 12;" in
  Logger.set_level ["Debug"; "Info"] ;
  let ast = parse_code code in
  let rec aux = function
    | [] ->
        ()
    | h :: t -> (
      match h with
      | Assign (_, v, expr) ->
          Logger.debug "Variable(%s) = Expression(%s)" v (expr_to_string expr) ;
          aux t
      | Expression (_, expr) ->
          Logger.debug "Expression(%s)" (expr_to_string expr) ;
          aux t )
  in
  aux ast
