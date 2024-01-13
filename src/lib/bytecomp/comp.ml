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

open Ast
open Types

let binop_comp : Base.binop_type -> Opcode.t =
  let open Base in
  let bin_operator : binary_operator -> Opcode.t = function
    | Plus ->
        [Opcode.ADD(1, 1, 0)]
    | Minus ->
        [Opcode.SUB(1, 1, 0)]
    | Multiply ->
        [Opcode.MUL(1, 1, 0)]
    | Divide ->
        [Opcode.DIV(1, 1, 0)]
    | Mod ->
        [Opcode.MOD(1, 1, 0)]
  in
  let bin_comparison : binary_comp -> Opcode.t = function
    | Equal | NotEqual | GEQ | LEQ | Greater | Lesser ->
        [Opcode.HALT]
  in
  function
  | `Operator op ->
      bin_operator op
  | `Compare op ->
      bin_comparison op
  | `Cons ->
      [Opcode.HALT]

let const_comp : Base.value -> Opcode.t = fun v -> [Opcode.VALUE v]

let var_comp : Base.value -> Opcode.t = function _ -> Opcode.empty

let rec expr_comp : Base.expr -> Opcode.t =
  let open Base in
  function
  | BinOp (op, left, right) ->
      let left = expr_comp left in
      let right = expr_comp right in
      let op = binop_comp op in
      left @ right @ op
  | Terminal (Const _ as v) ->
      const_comp v
  | Terminal (V_Var _ as v) ->
      var_comp v
  | _ ->
      Opcode.empty (* TODO: Function call *)

let rec stmt_comp : Tree.statement -> Opcode.t =
  let open Tree in
  function
  | Return (_, e) ->
      let e = expr_comp e in
      e @ [Opcode.RET]
  | Assign (_, v, e) ->
      let e = expr_comp e in
      let v = var_comp (Base.V_Var v) in
      e @ v @ [Opcode.STORE (0, 0)]
  | Block (_, stmts) ->
      let encapsulate l = [Opcode.SCP_DUPLICATE] @ l @ [Opcode.SCP_CLEAR] in
      let stmts = List.map stmt_comp stmts in
      encapsulate (List.concat stmts)
  | If (_, _, _, _) ->
      Opcode.empty
  | Expression (_, e, _) ->
      expr_comp e
  | _ ->
      Opcode.empty

let bytecomp (prog : Tree.program) : Opcode.t =
  let rec aux (prog : Tree.program) (acc : Opcode.t) : Opcode.t =
    match prog with [] -> acc | stmt :: t -> aux t (acc @ stmt_comp stmt)
  in
  aux prog Opcode.empty
