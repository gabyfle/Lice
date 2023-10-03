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

module Type = struct
  type variable = string

  type binary_operator =
    | Plus
    | Minus
    | Divide
    | Multiply
    | And
    | Or

  type expr =
    | Variable of variable
    | FunctionCall of string * expr
    | BinOp of binary_operator * expr * expr
    | Branch of expr * expr * expr option (* if (expr) then expr else [Some(expr) or None] *)
    | Return of expr
  
  and pattern =
    | Number of float
    | Bool of bool
    | WildCard
    | Tuple of pattern list

  type statement =
    | Assign of variable * expr
    | Expression of expr
    | FunctionDefinition of variable * variable list * statement list
    | MatchStatement of expr * (pattern * statement list) list

  type program = statement list
end
