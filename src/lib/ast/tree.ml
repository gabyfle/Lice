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

type statement =
  | Return of location * expr
  | Expression of location * expr * typ
  | Block of location * statement list
  | Assign of location * identificator * expr
  | FuncDef of location * identificator * identificator list * statement list
  | Match of location * expr * (expr * statement list) list
  | If of location * expr * statement * statement
  | ModuleDef of location * ident * statement list

and program = statement list

let val_to_typ = function
  | Terminal t ->
      Value.to_typ t
  | BinOp (_, _, _) ->
      T_Number
  | FuncCall (_, _) ->
      T_Auto

let stmt_to_string = function
  | Return (_, _) ->
      "Return"
  | Expression (_, _, _) ->
      "Expression"
  | Block (_, _) ->
      "Block"
  | Assign (_, _, _) ->
      "Assign"
  | FuncDef (_, _, _, _) ->
      "Function definition"
  | Match (_, _, _) ->
      "Pattern-matching"
  | If (_, _, _, _) ->
      "If statement"
  | ModuleDef (_, _, _) ->
      "Module definition"

let bincomp_to_string = function
  | Equal ->
      "=="
  | NotEqual ->
      "!="
  | GEQ ->
      ">="
  | LEQ ->
      "<="
  | Greater ->
      ">"
  | Lesser ->
      "<"

let binop_to_string = function
  | Plus ->
      "+"
  | Minus ->
      "-"
  | Divide ->
      "/"
  | Multiply ->
      "*"
  | Mod ->
      "%"

let get_location = function
  | Return (loc, _) ->
      loc
  | Expression (loc, _, _) ->
      loc
  | Block (loc, _) ->
      loc
  | Assign (loc, _, _) ->
      loc
  | FuncDef (loc, _, _, _) ->
      loc
  | Match (loc, _, _) ->
      loc
  | If (loc, _, _, _) ->
      loc
  | ModuleDef (loc, _, _) ->
      loc
