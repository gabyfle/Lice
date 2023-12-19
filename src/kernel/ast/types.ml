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

and typ = T_Number | T_String | T_List | T_Boolean | T_Auto | T_Void

and typed_ident = identificator * typ

type binary_operator = Plus | Minus | Divide | Multiply | Mod

type binary_comp = Equal | NotEqual | GEQ | LEQ | Greater | Lesser

type binop_type = [`Compare of binary_comp | `Operator of binary_operator]

type expr =
  | Empty
  | Number of float
  | String of string
  | Boolean of bool
  | List of expr option * expr
  | Variable of typed_ident
  | BinOp of binop_type * expr * expr
  | FuncCall of identificator * expr list

and statement =
  | Return of location * expr
  | Expression of location * expr * typ
  | Block of location * statement list
  | Assign of location * typed_ident * expr
  | FuncDef of location * typed_ident * typed_ident list * statement
  | Match of location * expr * (expr * statement list) list
  | If of location * expr * statement * statement

and program = statement list

let val_to_typ = function
  | Number _ ->
      T_Number
  | String _ ->
      T_String
  | Boolean _ ->
      T_Boolean
  | List _ ->
      T_List
  | Variable (_, t) ->
      t
  | BinOp (_, _, _) ->
      T_Number
  | FuncCall (_, _) ->
      T_Auto
  | Empty ->
      T_Void

let typ_to_string = function
  | T_Number ->
      "number"
  | T_String ->
      "string"
  | T_List ->
      "list"
  | T_Boolean ->
      "boolean"
  | T_Auto ->
      "auto"
  | T_Void ->
      "void"

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

let rec compare_expr e1 e2 =
  match (e1, e2) with
  | Empty, Empty ->
      true
  | Number f, Number f' ->
      f = f'
  | String s, String s' ->
      s = s'
  | Boolean b, Boolean b' ->
      b = b'
  | List (e1_opt, e1'), List (e2_opt, e2') ->
      ( match (e1_opt, e2_opt) with
      | Some e1_opt', Some e2_opt' ->
          compare_expr e1_opt' e2_opt'
      | None, None ->
          true
      | _ ->
          false )
      && compare_expr e1' e2'
  | Variable (id, _), Variable (id', _) ->
      id = id' (* assuming typed_ident can be compared directly *)
  | BinOp (op1, e1_1, e1_2), BinOp (op2, e2_1, e2_2) ->
      op1 = op2 && compare_expr e1_1 e2_1 && compare_expr e1_2 e2_2
  | FuncCall (id, exprs1), FuncCall (id', exprs2) ->
      id = id' && List.for_all2 compare_expr exprs1 exprs2
  | _ ->
      false
