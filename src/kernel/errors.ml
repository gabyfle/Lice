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
open Utils
open Utils.Logger
open Located_error

(* handle_type_exception is an helper function that allows to display nice error
   messages and to handle Located_error exception during the type checking
   process. *)
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
