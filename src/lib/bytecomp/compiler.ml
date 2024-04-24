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

let not_found (a : Base.t) (chunk : Chunk.t) : (Chunk.t * int) option =
  match a with
  | V_Variable _ ->
      None
  | _ ->
      let chunk, key = Chunk.add chunk a in
      if key = -1 then None else Some (chunk, key)

let find_key (a : Base.t) (chunk : Chunk.t) : (Chunk.t * int) option =
  let key = Chunk.get_key chunk a in
  match key with Some key -> Some (chunk, key) | None -> not_found a chunk

let load_value (a : Base.t) (chunk : Chunk.t) : Chunk.t * int =
  match a with
  | V_Variable _ -> (
      let res = find_key a chunk in
      match res with
      | Some (chunk, key) ->
          (Chunk.add_code chunk [Opcode.SEARCH key], 5)
      | None ->
          failwith "Variable not found" )
  | V_Boolean b ->
      (Chunk.add_code chunk [Opcode.LDBOL b], 2)
  | V_Void ->
      (Chunk.add_code chunk [Opcode.LDVOID], 1)
  | _ -> (
      let res = find_key a chunk in
      match res with
      | Some (chunk, key) ->
          (Chunk.add_code chunk [Opcode.LOADK key], 5)
      | None ->
          failwith "Variable not found" )

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

and compile_expr (expr : Base.expr) (chunk : Chunk.t) (curr_size : int) :
    Chunk.t * int =
  match expr with
  | Terminal t ->
      let chunk, size = load_value t chunk in
      (chunk, curr_size + size)
  | BinOp (op, a, b) ->
      let chunk, bsize = compile_expr b chunk curr_size in
      let chunk = Chunk.add_code chunk [Opcode.PUSH] in
      let chunk, asize = compile_expr a chunk (bsize + 1) in
      let chunk, opsize = compile_operator op chunk in
      (chunk, asize + opsize)
  | FuncCall (name, args) ->
      let funck =
        match Chunk.get_key chunk (V_Variable name) with
        | Some key ->
            key
        | None ->
            failwith "Function not found"
      in
      let func = Chunk.get chunk funck in
      let address =
        match func with
        | V_Function a ->
            Lfunction.address a
        | _ ->
            failwith "Not a function"
      in
      let rec compile_args (chunk : Chunk.t) (size : int) = function
        | [] ->
            (chunk, size)
        | arg :: t ->
            let chunk, size = compile_expr arg chunk size in
            let chunk = Chunk.add_code chunk [Opcode.PUSH] in
            compile_args chunk (size + 1) t
      in
      let chunk, size = compile_args chunk curr_size args in
      let chunk = Chunk.add_code chunk [Opcode.CALL (Int32.to_int address)] in
      (chunk, size + 5)

and compile_return (expr : Base.expr) (chunk : Chunk.t) (curr_size : int) :
    Chunk.t * int =
  let chunk, size = compile_expr expr chunk curr_size in
  let opcodes = Opcode.empty in
  let opcodes = Opcode.add opcodes Opcode.RETURN in
  let opcodes = Opcode.add opcodes Opcode.PUSH in
  (Chunk.add_code chunk opcodes, size + 2)

and compile_block (block : statement list) (chunk : Chunk.t) (curr_size : int) :
    Chunk.t * int =
  let rec aux (block : statement list) (chunk : Chunk.t) (csize : int) :
      Chunk.t * int =
    match block with
    | [] ->
        (chunk, csize)
    | stmt :: t ->
        let chunk, size = compile_statement stmt chunk csize in
        let chunk, tsize = aux t chunk size in
        (chunk, tsize)
  in
  let chunk = Chunk.add_code chunk [Opcode.PUSHENV] in
  let chunk, size = aux block chunk (curr_size + 1) in
  let chunk = Chunk.add_code chunk [Opcode.POPENV] in
  (chunk, size + 1)

and compile_assign (var : Base.identificator) (expr : Base.expr)
    (chunk : Chunk.t) (curr_size : int) : Chunk.t * int =
  let var = Base.V_Variable var in
  let res = find_key var chunk in
  match res with
  | Some (chunk, key) ->
      let chunk, size = compile_expr expr chunk curr_size in
      let opcodes = Opcode.empty in
      let opcodes = Opcode.add opcodes (Opcode.EXTEND key) in
      (Chunk.add_code chunk opcodes, size + 5)
  | None ->
      let chunk, key = Chunk.add chunk var in
      let chunk, size = compile_expr expr chunk curr_size in
      let opcodes = Opcode.empty in
      let opcodes = Opcode.add opcodes (Opcode.EXTEND key) in
      (Chunk.add_code chunk opcodes, size + 5)

and compile_if (cond : Base.expr) (then_ : statement) (else_ : statement)
    (chunk : Chunk.t) (curr_size : int) : Chunk.t * int =
  let chunk, curr_size = compile_expr cond chunk curr_size in
  let opcodes = Opcode.empty in
  (* We're going to make a copy of the current chunk to compile the then / else
     expression *)
  let tchunk = chunk in
  (* This new chunk has empty code, so that we can append it to the original
     chunk later *)
  let tchunk = Chunk.set tchunk Opcode.empty in
  let tchunk, tsize = compile_statement then_ tchunk curr_size in
  let echunk = chunk in
  let echunk = Chunk.set echunk Opcode.empty in
  let echunk, esize = compile_statement else_ echunk (tsize + 10) in
  let tchunk = Chunk.add_code tchunk [Opcode.JMP esize] in
  let tchunk = Chunk.add_code tchunk (List.rev (Chunk.code echunk)) in
  let opcodes = Opcode.add opcodes (Opcode.JMPNZ (tsize + 10)) in
  (* + 10 here is for the recently added JMP operation + JMPNZ *)
  let chunk = Chunk.add_code chunk opcodes in
  let chunk = Chunk.add_code chunk (List.rev (Chunk.code tchunk)) in
  (chunk, esize)

and compile_match (pattern : Base.expr)
    (cases : (Base.expr * statement list) list) (chunk : Chunk.t)
    (curr_size : int) : Chunk.t * int =
  let chunk, psize = compile_expr pattern chunk curr_size in
  let chunk = Chunk.add_code chunk [Opcode.PUSH] in
  let rec aux (cases : (Base.expr * statement list) list) (chunk : Chunk.t)
      (curr_size : int) : Chunk.t * int =
    match cases with
    | [] ->
        (chunk, 0)
    | case :: t ->
        let expr, stmt = case in
        (* the pattern is pushed into the stack *)
        let chunk, csize = compile_expr expr chunk curr_size in
        (* the expr will then be into the acc *)
        let chunk = Chunk.add_code chunk [Opcode.EQ] in
        (* so we can compare them with EQ *)
        let bchunk = chunk in
        let bchunk = Chunk.set bchunk Opcode.empty in
        let bchunk, ssize =
          compile_statement (Block (Lexing.dummy_pos, stmt)) bchunk (csize + 1)
        in
        let chunk = Chunk.add_code chunk [Opcode.JMPNZ ssize] in
        let chunk = Chunk.add_code chunk (Chunk.code bchunk) in
        let chunk, tsize = aux t chunk (psize + 5) in
        (chunk, tsize)
  in
  aux cases chunk (psize + 1)

and compile_function (id : Base.identificator) (vars : Base.identificator list)
    (stmt : statement) (chunk : Chunk.t) (curr_size : int) : Chunk.t * int =
  let chunk = chunk in
  let chunk = Chunk.set chunk Opcode.empty in
  let chunk, key = Chunk.add chunk (V_Function (Int32.of_int curr_size)) in
  let chunk, _ = Chunk.addk chunk (V_Variable id) key in
  let rec get_locals (chunk : Chunk.t) (curr_size : int) = function
    | [] ->
        (chunk, curr_size)
    | var :: t ->
        let chunk, key = Chunk.add chunk (V_Variable var) in
        let chunk = Chunk.add_code chunk [POP; Opcode.EXTEND key] in
        get_locals chunk (curr_size + 6) t
  in
  let chunk, curr_size = get_locals chunk curr_size vars in
  let chunk, size = compile_statement stmt chunk curr_size in
  let chunk = Chunk.emplace chunk true in
  (chunk, size)

and compile_statement (stmt : statement) (chunk : Chunk.t) (curr_size : int) :
    Chunk.t * int =
  match stmt with
  | Return (_, expr) ->
      compile_return expr chunk curr_size
  | Block (_, block) ->
      compile_block block chunk curr_size
  | Assign (_, var, expr) ->
      compile_assign var expr chunk curr_size
  | If (_, cond, then_, else_) ->
      compile_if cond then_ else_ chunk curr_size
  | Match (_, pattern, cases) ->
      compile_match pattern cases chunk curr_size
  | FuncDef (_, id, vars, stmt) ->
      compile_function id vars stmt chunk curr_size
  | _ ->
      (chunk, curr_size)

let compile (ast : program) : Bytes.t =
  let code = Code.empty in
  let rec aux (ast : program) (code : Code.t) (curr_size : int) : Code.t =
    match ast with
    | [] ->
        code
    | stmt :: t ->
        let chunk = Option.value (Code.get code) ~default:Chunk.empty in
        let chunk, size = compile_statement stmt chunk curr_size in
        if Chunk.emplaced chunk then
          let code = Code.add chunk code in
          let chunk' = Option.value (Code.get code) ~default:Chunk.empty in
          let chunk = Chunk.copy_hd chunk chunk' in
          let code = Code.set chunk code in
          aux t code size
        else
          let code = Code.set chunk code in
          aux t code size
  in
  let code = aux ast code 0 in
  let chunk = Option.value (Code.get code) ~default:Chunk.empty in
  let chunk = Chunk.add_code chunk [Opcode.HALT] in
  let code = Code.set chunk code in
  Code.emit code
