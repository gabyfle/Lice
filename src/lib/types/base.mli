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

type ident = string

type binary_operator = Plus | Minus | Divide | Multiply | Mod

type binary_comp = Equal | NotEqual | GEQ | LEQ | Greater | Lesser

type binop_type =
  [`Compare of binary_comp | `Operator of binary_operator | `Cons]

type typed_ident = ident * typ

and typ = T_Number | T_String | T_List | T_Boolean | T_Auto | T_Void

type identificator = [`Ident of typed_ident | `Module of ident * typed_ident]

type expr =
  | Terminal of t
  | BinOp of binop_type * expr * expr
  | FuncCall of identificator * expr list

and t =
  | V_Number of Lnumber.t
  | V_String of Lstring.t
  | V_List of expr list
  | V_Boolean of Lbool.t
  | V_Function of Lfunction.t
  | V_Variable of identificator
  | V_Void

val identificator_to_string : identificator -> string

val string_to_identificator : string -> identificator

val bincomp_to_string : binary_comp -> string

val binop_to_string : binary_operator -> string

val binop_type_to_string : binop_type -> string
