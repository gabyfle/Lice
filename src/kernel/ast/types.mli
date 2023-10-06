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

type variable = string

type binary_operator = Plus | Minus | Divide | Multiply | Mod

type expr =
  | Number of float
  | Variable of variable
  | BinOp of binary_operator * expr * expr
  | Assign of variable * expr

and statement =
  | Expression of location * expr
  | Block of location * statement list

and program = statement list

module type IDENT = sig
  type t

  val to_string : t -> string

  val of_string : string -> t

  val ( = ) : t -> t -> bool
end

module Variable : IDENT

val expr_to_string : expr -> string
