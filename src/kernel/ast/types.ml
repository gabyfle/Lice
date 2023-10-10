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

type location = Lexing.position

type identificator = string

and typ =
  | T_Number
  | T_String
  | T_Map
  | T_List
  | T_Boolean
  | T_Auto

and typed_ident = identificator * typ

type binary_operator = Plus | Minus | Divide | Multiply | Mod

type expr =
  | Number of float
  | Variable of typed_ident
  | BinOp of binary_operator * expr * expr
  | Assign of typed_ident * expr
  | FuncCall of typed_ident * typed_ident list
  | Return of expr

and statement =
  | Expression of location * expr
  | Block of location * statement list
  | FuncDef of location * typed_ident * typed_ident list * statement

and program = statement list
