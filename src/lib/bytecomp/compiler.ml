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
open Types

let not_found (a : Base.t) (chunk : Chunk.t) : Chunk.t * int =
  match a with
  | V_Variable v ->
      failwith ("Variable " ^ Base.identificator_to_string v ^ " not defined")
  | _ -> (
      let chunk = Chunk.add chunk a in
      let key = Chunk.get_key chunk a in
      match key with
      | Some key ->
          (chunk, key)
      | None ->
          failwith "Internal error: key not found" )

let find_key (a : Base.t) (chunk : Chunk.t) : Chunk.t * int =
  let key = Chunk.get_key chunk a in
  match key with Some key -> (chunk, key) | None -> not_found a chunk

let load_value (a : Base.t) (key : int) : Opcode.opcode * int =
  match a with
  | V_Variable _ ->
      (Opcode.SEARCH key, 5)
  | V_Boolean b ->
      (Opcode.LDBOL b, 2)
  | _ ->
      (Opcode.LOADK key, 5)

let compile_bin (op : Base.binary_operator) (chunk : Chunk.t) : Chunk.t * int =
  let opcodes = Opcode.empty in
  let opcodes =
    match op with
    | Plus ->
        Opcode.add opcodes Opcode.ADD
    | Minus ->
        Opcode.add opcodes Opcode.SUB
    | Multiply ->
        Opcode.add opcodes Opcode.MUL
    | Divide ->
        Opcode.add opcodes Opcode.DIV
    | Mod ->
        Opcode.add opcodes Opcode.MOD
  in
  (Chunk.add_code chunk opcodes, 1)

let compile_cmp (op : Base.binary_comp) (chunk : Chunk.t) : Chunk.t * int =
  let opcodes = Opcode.empty in
  let opcodes =
    match op with
    | Equal ->
        Opcode.add opcodes Opcode.EQ
    | NotEqual ->
        Opcode.add opcodes Opcode.NEQ
    | LEQ ->
        Opcode.add opcodes Opcode.LE
    | GEQ ->
        Opcode.add opcodes Opcode.GE
    | Lesser ->
        Opcode.add opcodes Opcode.LT
    | Greater ->
        Opcode.add opcodes Opcode.GT
  in
  (Chunk.add_code chunk opcodes, 1)

let rec compile_operator (op : Base.binop_type) (chunk : Chunk.t) :
    Chunk.t * int =
  match op with
  | `Compare op ->
      compile_cmp op chunk
  | `Operator op ->
      compile_bin op chunk
  | `Cons ->
      compile_bin Base.Plus chunk

and compile_expr (expr : Base.expr) (chunk : Chunk.t) : Chunk.t * int =
  match expr with
  | Terminal t ->
      let chunk, key = find_key t chunk in
      let opcode, size = load_value t key in
      (Chunk.add_code chunk [opcode], size)
  | BinOp (op, a, b) ->
      let chunk, bsize = compile_expr b chunk in
      let chunk = Chunk.add_code chunk [Opcode.PUSH] in
      let chunk, asize = compile_expr a chunk in
      let chunk, opsize = compile_operator op chunk in
      (chunk, asize + bsize + opsize + 1)
  | _ ->
      (chunk, 0)

and compile_return (expr : Base.expr) (chunk : Chunk.t) : Chunk.t * int =
  let chunk, size = compile_expr expr chunk in
  let opcodes = Opcode.empty in
  let opcodes = Opcode.add opcodes Opcode.PUSH in
  let opcodes = Opcode.add opcodes Opcode.RETURN in
  (Chunk.add_code chunk opcodes, size + 2)

and compile_block (block : statement list) (chunk : Chunk.t) : Chunk.t * int =
  let rec aux (block : statement list) (chunk : Chunk.t) : Chunk.t * int =
    match block with
    | [] ->
        (chunk, 0)
    | stmt :: t ->
        let chunk, size = compile_statement stmt chunk in
        let chunk, tsize = aux t chunk in
        (chunk, size + tsize)
  in
  let chunk = Chunk.add_code chunk [Opcode.PUSHENV] in
  let chunk, size = aux block chunk in
  let chunk = Chunk.add_code chunk [Opcode.POPENV] in
  (chunk, size + 2)

and compile_assign (var : Base.identificator) (expr : Base.expr)
    (chunk : Chunk.t) : Chunk.t * int =
  let var = Base.V_Variable var in
  let chunk = Chunk.add chunk var in
  let chunk, key = find_key var chunk in
  let chunk, size = compile_expr expr chunk in
  let opcodes = Opcode.empty in
  let opcodes = Opcode.add opcodes (Opcode.EXTEND key) in
  (Chunk.add_code chunk opcodes, size + 5)

and compile_if (cond : Base.expr) (then_ : statement) (else_ : statement)
    (chunk : Chunk.t) : Chunk.t * int =
  let chunk, exprsize = compile_expr cond chunk in
  let opcodes = Opcode.empty in
  let chunk, tsize = compile_statement then_ chunk in
  let opcodes = Opcode.add opcodes (Opcode.JMPNZ tsize) in
  let chunk = Chunk.add_code chunk opcodes in
  let chunk, esize = compile_statement else_ chunk in
  (chunk, exprsize + tsize + esize + 5)

and compile_statement (stmt : statement) (chunk : Chunk.t) : Chunk.t * int =
  match stmt with
  | Return (_, expr) ->
      compile_return expr chunk
  | Block (_, block) ->
      compile_block block chunk
  | Assign (_, var, expr) ->
      compile_assign var expr chunk
  | If (_, cond, then_, else_) ->
      compile_if cond then_ else_ chunk
  | _ ->
      (chunk, 0)

let compile (ast : program) : Chunk.t =
  let chunk = Chunk.empty in
  let rec aux (ast : program) (chunk : Chunk.t) : Chunk.t =
    match ast with
    | [] ->
        chunk
    | stmt :: t ->
        let chunk, _ = compile_statement stmt chunk in
        aux t chunk
  in
  aux ast chunk
