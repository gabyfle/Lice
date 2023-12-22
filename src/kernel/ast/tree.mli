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
open Types.Base

type location = Lexing.position

type identificator = string

and typed_ident = identificator * typ

type statement =
  | Return of location * expr
  | Expression of location * expr * typ
  | Block of location * statement list
  | Assign of location * typed_ident * expr
  | FuncDef of location * typed_ident * typed_ident list * statement
  | Match of location * expr * (expr * statement list) list
  | If of location * expr * statement * statement

and program = statement list

val val_to_typ : expr -> Base.typ

val stmt_to_string : statement -> string

val bincomp_to_string : binary_comp -> string

val binop_to_string : binary_operator -> string
