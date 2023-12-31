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
open Types.Base

type exception_type =
  [ `Language_Error of string
  | `Not_Integer
  | `Not_Number
  | `Division_by_zero
  | `Undefined_Function
  | `Undefined_Variable of identificator
  | `Op_Mismatch of string * typ * typ
  | `Not_A_Callable
  | `Function_Value
  | `Wrong_Parameters_Number of identificator * int * int
  | `Wrong_Parameter_Type of identificator * typ * typ
  | `Wrong_Assign_Type of identificator * typ * typ
  | `Wrong_Return_Type of identificator * typ * typ
  | `Wrong_Case_Type of typ * typ
  | `Wrong_Type of typ * typ
  | `Expected_Return_Statement ]

exception Located_error of exception_type * location

(* handle_type_exception is an helper function that allows to display nice error
   messages and to handle Located_error exception during the type checking
   process. *)
val handle_type_exception : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
