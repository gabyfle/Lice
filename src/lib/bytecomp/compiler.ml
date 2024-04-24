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
open Utils.Logger

module Worker = struct
  module JumpTable = Map.Make (String)

  type t = {chunk: Chunk.t; size: int; jump_table: int JumpTable.t}

  let empty = {chunk= Chunk.empty; size= 0; jump_table= JumpTable.empty}

  let chunk (t : t) = t.chunk

  let size (t : t) = t.size

  let grow (t : t) (size : int) : t = {t with size= t.size + size}

  let addjmp (t : t) (key : string) (value : int) : t =
    {t with jump_table= JumpTable.add key value t.jump_table}
end

module Code = struct
  type t = Worker.t list

  let empty = []

  let add (chunk : Worker.t) (t : t) = List.append t [chunk]

  let get (t : t) = match t with [] -> None | h :: _ -> Some h

  let set (chunk : Worker.t) (code : t) : t =
    match code with [] -> [chunk] | _ :: t -> chunk :: t

  let split (t : t) : Worker.t option * t =
    match t with [] -> (None, []) | h :: t -> (Some h, t)

  let emit (t : t) =
    let main, rest = split t in
    let rec aux acc = function
      | [] ->
          acc
      | chunk :: t ->
          aux (Bytes.cat acc (Chunk.emit_code (Worker.chunk chunk))) t
    in
    let bytes = aux Bytes.empty rest in
    let main = Option.value main ~default:Worker.empty in
    let main = Chunk.emit (Worker.chunk main) in
    Bytes.cat main bytes
end

let add_key (a : Base.t) (worker : Worker.t) : Worker.t * int =
  Value.pretty Format.str_formatter a ;
  Logger.info "Adding key %s" (Format.flush_str_formatter ()) ;
  let chunk, key = Chunk.add (Worker.chunk worker) a in
  if key = -1 then failwith "Key not found" else ({worker with chunk}, key)

let find_key (a : Base.t) (worker : Worker.t) : Worker.t * int option =
  let key = Chunk.get_key (Worker.chunk worker) a in
  (worker, key)

let load_value (a : Base.t) (worker : Worker.t) : Worker.t =
  match a with
  | V_Variable _ -> (
      let worker, res = find_key a worker in
      match res with
      | Some key ->
          let worker =
            Worker.grow
              { worker with
                chunk= Chunk.add_code (Worker.chunk worker) [Opcode.SEARCH key]
              }
              5
          in
          worker
      | None ->
          failwith "Variable here not found" )
  | V_Boolean b ->
      Worker.grow
        { worker with
          chunk= Chunk.add_code (Worker.chunk worker) [Opcode.LDBOL b] }
        2
  | V_Void ->
      Worker.grow
        {worker with chunk= Chunk.add_code (Worker.chunk worker) [Opcode.LDVOID]}
        1
  | _ -> (
      let worker, res = find_key a worker in
      match res with
      | Some key ->
          Worker.grow
            { worker with
              chunk= Chunk.add_code (Worker.chunk worker) [Opcode.LOADK key] }
            5
      | None ->
          let worker, key = add_key a worker in
          Worker.grow
            { worker with
              chunk= Chunk.add_code (Worker.chunk worker) [Opcode.LOADK key] }
            5 )

let compile_bin (op : Base.binary_operator) (worker : Worker.t) : Worker.t =
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
  Worker.grow
    {worker with chunk= Chunk.add_code (Worker.chunk worker) opcodes}
    1

let compile_cmp (op : Base.binary_comp) (worker : Worker.t) : Worker.t =
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
  Worker.grow
    {worker with chunk= Chunk.add_code (Worker.chunk worker) opcodes}
    1

let rec compile_operator (op : Base.binop_type) (worker : Worker.t) : Worker.t =
  match op with
  | `Compare op ->
      compile_cmp op worker
  | `Operator op ->
      compile_bin op worker
  | `Cons ->
      compile_bin Base.Plus worker

and compile_stmt_list (stmts : statement list) (worker : Worker.t) : Worker.t =
  match stmts with
  | [] ->
      worker
  | stmt :: t ->
      let worker = compile_statement stmt worker in
      compile_stmt_list t worker

and compile_expr (expr : Base.expr) (worker : Worker.t) : Worker.t =
  match expr with
  | Terminal t ->
      load_value t worker
  | BinOp (op, a, b) ->
      let worker = compile_expr b worker in
      let chunk = Worker.chunk worker in
      let chunk = Chunk.add_code chunk [Opcode.PUSH] in
      let worker = Worker.grow {worker with chunk} 1 in
      let worker = compile_expr a worker in
      compile_operator op worker
  | FuncCall (name, args) ->
      let funck =
        match Chunk.get_key (Worker.chunk worker) (V_Variable name) with
        | Some key ->
            key
        | None ->
            failwith "Function not found"
      in
      let rec compile_args (worker : Worker.t) = function
        | [] ->
            worker
        | arg :: t ->
            let worker = compile_expr arg worker in
            let worker =
              Worker.grow
                { worker with
                  chunk= Chunk.add_code (Worker.chunk worker) [Opcode.PUSH] }
                1
            in
            compile_args worker t
      in
      let worker = compile_args worker args in
      let worker =
        Worker.grow
          { worker with
            chunk= Chunk.add_code (Worker.chunk worker) [Opcode.LOADK funck] }
          5
      in
      Worker.grow
        { worker with
          chunk=
            Chunk.add_code (Worker.chunk worker) [Opcode.CALL (List.length args)]
        }
        5

and compile_return (expr : Base.expr) (worker : Worker.t) : Worker.t =
  let worker = compile_expr expr worker in
  let opcodes = Opcode.empty in
  let opcodes = Opcode.add opcodes Opcode.PUSH in
  let opcodes = Opcode.add opcodes Opcode.POPENV in
  let opcodes = Opcode.add opcodes Opcode.RETURN in
  let chunk = Worker.chunk worker in
  let chunk = Chunk.add_code chunk opcodes in
  Worker.grow {worker with chunk} 3

and compile_block (block : statement list) (worker : Worker.t) : Worker.t =
  let chunk = Worker.chunk worker in
  let chunk = Chunk.add_code chunk [Opcode.PUSHENV] in
  let worker = Worker.grow {worker with chunk} 1 in
  let worker = compile_stmt_list block worker in
  let chunk = Worker.chunk worker in
  let chunk = Chunk.add_code chunk [Opcode.POPENV] in
  Worker.grow {worker with chunk} 1

and compile_assign (var : Base.identificator) (expr : Base.expr)
    (worker : Worker.t) : Worker.t =
  let var = Base.V_Variable var in
  let worker, res = find_key var worker in
  match res with
  | Some key ->
      let worker = compile_expr expr worker in
      let opcodes = Opcode.add Opcode.empty (Opcode.EXTEND key) in
      let chunk = Worker.chunk worker in
      Worker.grow {worker with chunk= Chunk.add_code chunk opcodes} 5
  | None ->
      let chunk, key = Chunk.add (Worker.chunk worker) var in
      let worker = {worker with chunk} in
      let worker = compile_expr expr worker in
      let opcodes = Opcode.add Opcode.empty (Opcode.EXTEND key) in
      let chunk = Worker.chunk worker in
      Worker.grow {worker with chunk= Chunk.add_code chunk opcodes} 5

and compile_if (cond : Base.expr) (then_ : statement) (else_ : statement)
    (worker : Worker.t) : Worker.t =
  let worker = compile_expr cond worker in
  let opcodes = Opcode.empty in
  (* We're going to make a copy of the current chunk to compile the then / else
     expression *)
  let tchunk = Worker.chunk worker in
  (* This new chunk has empty code, so that we can append it to the original
     chunk later *)
  let tchunk = Chunk.set tchunk Opcode.empty in
  let tworker = {worker with chunk= tchunk} in
  let tworker = compile_statement then_ tworker in
  let echunk = Worker.chunk worker in
  let echunk = Chunk.set echunk Opcode.empty in
  let eworker = {tworker with chunk= echunk} in
  let eworker = compile_statement else_ (Worker.grow eworker 10) in
  let opcodes = Opcode.add opcodes (Opcode.JMPNZ (Worker.size tworker + 10)) in
  let tchunk =
    Chunk.add_code (Worker.chunk tworker) [Opcode.JMP (Worker.size eworker)]
  in
  let tchunk =
    Chunk.add_code tchunk (List.rev (Chunk.code (Worker.chunk eworker)))
  in
  let worker =
    {worker with chunk= Chunk.add_code (Worker.chunk worker) opcodes}
  in
  { worker with
    chunk= Chunk.add_code (Worker.chunk worker) (List.rev (Chunk.code tchunk))
  }

and compile_match (pattern : Base.expr)
    (cases : (Base.expr * statement list) list) (worker : Worker.t) : Worker.t =
  let worker = compile_expr pattern worker in
  let worker =
    Worker.grow
      {worker with chunk= Chunk.add_code (Worker.chunk worker) [Opcode.PUSH]}
      1
  in
  let rec aux (cases : (Base.expr * statement list) list) (worker : Worker.t) :
      Worker.t =
    match cases with
    | [] ->
        worker
    | case :: t ->
        let expr, stmt = case in
        (* the pattern is pushed into the stack *)
        let worker = compile_expr expr worker in
        (* the expr will then be into the acc *)
        let worker =
          Worker.grow
            {worker with chunk= Chunk.add_code (Worker.chunk worker) [Opcode.EQ]}
            1
        in
        (* so we can compare them with EQ *)
        let bworker = worker in
        let bchunk = Chunk.set (Worker.chunk bworker) Opcode.empty in
        let bworker =
          compile_statement
            (Block (Lexing.dummy_pos, stmt))
            (Worker.grow {worker with chunk= bchunk} 1)
        in
        let worker =
          Worker.grow
            { worker with
              chunk=
                Chunk.add_code (Worker.chunk worker)
                  [Opcode.JMPNZ (Worker.size bworker)] }
            5
        in
        aux t
          { worker with
            chunk= Chunk.add_code (Worker.chunk worker) (Chunk.code bchunk) }
  in
  aux cases worker

and compile_function (id : Base.identificator) (vars : Base.identificator list)
    (stmts : statement list) (worker : Worker.t) : Worker.t =
  let worker =
    {worker with chunk= Chunk.set (Worker.chunk worker) Opcode.empty; size= 0}
  in
  let chunk, key =
    Chunk.add (Worker.chunk worker)
      (V_Function (Int32.of_int (Worker.size worker)))
  in
  let chunk, key = Chunk.addk chunk (V_Variable id) key in
  let worker =
    Worker.addjmp {worker with chunk} (Base.identificator_to_string id) key
  in
  let rec get_locals (worker : Worker.t) = function
    | [] ->
        worker
    | var :: t ->
        let chunk, key = Chunk.add (Worker.chunk worker) (V_Variable var) in
        get_locals
          (Worker.grow
             {worker with chunk= Chunk.add_code chunk [Opcode.EXTEND key; POP]}
             6 )
          t
  in
  let worker =
    Worker.grow
      {worker with chunk= Chunk.add_code (Worker.chunk worker) [Opcode.PUSHENV]}
      1
  in
  let worker = get_locals worker vars in
  let worker = compile_stmt_list stmts worker in
  {worker with chunk= Chunk.emplace (Worker.chunk worker) true}

and compile_statement (stmt : statement) (worker : Worker.t) : Worker.t =
  match stmt with
  | Return (_, expr) ->
      compile_return expr worker
  | Block (_, block) ->
      compile_block block worker
  | Assign (_, var, expr) ->
      compile_assign var expr worker
  | If (_, cond, then_, else_) ->
      compile_if cond then_ else_ worker
  | Match (_, pattern, cases) ->
      compile_match pattern cases worker
  | FuncDef (_, id, vars, stmt) ->
      compile_function id vars stmt worker
  | _ ->
      worker

let adjust_jumps (code : Opcode.t) (delta : int) : Opcode.t =
  Printf.printf "Adjusting delta %d\n" delta ;
  let rec aux acc = function
    | [] ->
        List.rev acc
    | Opcode.JMPNZ i :: t ->
        let i = i + delta in
        aux (Opcode.JMPNZ i :: acc) t
    | Opcode.JMP i :: t ->
        let i = i + delta in
        aux (Opcode.JMP i :: acc) t
    | h :: t ->
        aux (h :: acc) t
  in
  aux [] code

let resolve_addresses (code : Code.t) : Code.t =
  let main, rest = Code.split code in
  let main = Option.value main ~default:Worker.empty in
  let sizes =
    let rec aux acc = function
      | [] ->
          acc
      | worker :: t ->
          aux ((Worker.size worker, Some worker) :: acc) t
    in
    aux [(Worker.size main, None)] rest
  in
  let todo = ref [] in
  let iter (key : int) (value : Base.t) : unit =
    match value with V_Function _ -> todo := key :: !todo | _ -> ()
  in
  let rec aux (main : Worker.t) (size : int) (rest : Code.t) = function
    | [], [] ->
        (main, rest)
    | [], (dsize, worker) :: t' ->
        let worker =
          match worker with
          | Some worker ->
              let code = Chunk.code (Worker.chunk worker) in
              let code = adjust_jumps code size in
              let chunk = Chunk.set (Worker.chunk worker) code in
              let worker = {worker with chunk} in
              Code.add worker rest
          | None ->
              rest
        in
        aux main (dsize + size) worker ([], t')
    | key :: t, (dsize, worker) :: t' ->
        let main = Worker.grow main dsize in
        let main =
          { main with
            chunk=
              Chunk.setk (Worker.chunk main) key
                (V_Function (Int32.of_int (size + dsize))) }
        in
        let worker =
          match worker with
          | Some worker ->
              let code = Chunk.code (Worker.chunk worker) in
              let code = adjust_jumps code (size + dsize) in
              let chunk = Chunk.set (Worker.chunk worker) code in
              let worker = {worker with chunk} in
              Code.add worker rest
          | None ->
              rest
        in
        aux main (dsize + size) worker (t, t')
    | _ ->
        (main, rest)
  in
  Chunk.iter (Worker.chunk main) iter ;
  let main, rest = aux main 0 [] (!todo, List.rev sizes) in
  List.rev (Code.add main rest)

let compile (ast : program) : Bytes.t =
  let code = Code.empty in
  let rec aux (ast : program) (code : Code.t) : Code.t =
    match ast with
    | [] ->
        code
    | stmt :: t ->
        let main = Option.value (Code.get code) ~default:Worker.empty in
        let worker = compile_statement stmt main in
        if Chunk.emplaced (Worker.chunk worker) then
          (* if the compiled chunk needs to be emplaced, we copy its header into
             the main chunk but we make sure that the flag of the main chunk
             isn't changed to true *)
          let chunk = Worker.chunk worker in
          let chunk = Chunk.emplace chunk false in
          let main =
            {main with chunk= Chunk.merge_hd chunk (Worker.chunk main)}
          in
          let code = Code.set main code in
          let code = Code.add worker code in
          aux t code
        else
          (* else, we just add the code to the main chunk without carrying of
             anything *)
          let main =
            { main with
              chunk=
                Chunk.merge_hd
                  (Chunk.add_code (Worker.chunk main)
                     (Chunk.code (Worker.chunk worker)) )
                  (Worker.chunk worker)
            ; size= Worker.size worker }
          in
          let code = Code.set main code in
          aux t code
  in
  let code = aux ast code in
  let worker = Option.value (Code.get code) ~default:Worker.empty in
  let chunk = Worker.chunk worker in
  let chunk = Chunk.add_code chunk [Opcode.HALT] in
  let worker = Worker.grow {worker with chunk} 1 in
  let code = Code.set worker code in
  let code = resolve_addresses code in
  Code.emit code
