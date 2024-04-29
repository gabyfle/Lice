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
  type t = Worker.t option * Worker.t list

  let empty = (None, [])

  let add (chunk : Worker.t) ((h, t) : t) : t = (h, List.append t [chunk])

  let _add_front (chunk : Worker.t) ((h, t) : t) : t = (h, List.append [chunk] t)

  let get ((h, _) : t) = h

  let set (chunk : Worker.t) ((_, t) : t) : t = (Some chunk, t)

  let emit ((main, rest) : t) : Bytes.t =
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

let unique_id =
  let count = ref 0 in
  fun () -> incr count ; !count

let in_block = ref 0 (* Used to know if we're in a block *)

let returned = ref false (* Used to know if we just in a returned statement *)

let add_key (a : Base.t) (worker : Worker.t) : Worker.t * int =
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
      let worker =
        match Chunk.last (Worker.chunk worker) with
        | Opcode.CALL _ ->
            (* if the last instruction was a call to a function, args are
               already into the stack*)
            worker
        | _ ->
            let chunk = Chunk.add_code (Worker.chunk worker) [Opcode.PUSH] in
            Worker.grow {worker with chunk} 1
      in
      let worker = compile_expr a worker in
      let worker =
        match Chunk.last (Worker.chunk worker) with
        | Opcode.CALL _ ->
            (* if the last instruction was a call to a function, args are into
               the stack and we need to pop one into the acc *)
            let chunk = Chunk.add_code (Worker.chunk worker) [Opcode.POP] in
            Worker.grow {worker with chunk} 1
        | _ ->
            worker
      in
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
              (* in the same idea as when compiling binary operators, we need to
                 check if the last expression was a call to func *)
              match Chunk.last (Worker.chunk worker) with
              | Opcode.CALL _ ->
                  worker
              | _ ->
                  Worker.grow
                    { worker with
                      chunk= Chunk.add_code (Worker.chunk worker) [Opcode.PUSH]
                    }
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

and compile_return (expr : Base.expr) (nargs : int) (worker : Worker.t) :
    Worker.t =
  let worker = compile_expr expr worker in
  let opcodes = ref Opcode.empty in
  let grow = ref 0 in
  opcodes := Opcode.add !opcodes Opcode.PUSH ;
  for _ = 1 to !in_block do
    opcodes := Opcode.add !opcodes Opcode.POPENV ;
    returned := true ;
    grow := !grow + 1
  done ;
  in_block := 0 ;
  let opcodes = Opcode.add !opcodes Opcode.POPENV in
  let opcodes = Opcode.add opcodes (Opcode.RETURN nargs) in
  Worker.grow
    {worker with chunk= Chunk.add_code (Worker.chunk worker) opcodes}
    (7 + !grow)

and compile_block (block : statement list) (worker : Worker.t) : Worker.t =
  let worker =
    Worker.grow
      {worker with chunk= Chunk.add_code (Worker.chunk worker) [Opcode.PUSHENV]}
      1
  in
  in_block := !in_block + 1 ;
  let worker = compile_stmt_list block worker in
  let grow = if not !returned then 1 else 0 in
  Worker.grow
    { worker with
      chunk=
        ( if not !returned then
            Chunk.add_code (Worker.chunk worker) [Opcode.POPENV]
          else Worker.chunk worker ) }
    grow

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
  Printf.printf "Compiling if, init size: %de\n" (Worker.size worker) ;
  let worker = compile_expr cond worker in
  Printf.printf "Compiling if, after cond size: %de\n" (Worker.size worker) ;
  let empty = Opcode.empty in
  (* we need to compile the then statement to update correctly the size of the
     chunk then, compile the else statement in a separate chunk with the same
     header to have the correct symbols *)
  let then_ =
    compile_statement then_
      {worker with chunk= Chunk.set (Worker.chunk worker) empty; size= 0}
  in
  let tsize = Worker.size then_ in
  let worker =
    (* we're adding the JUMP operation with the good size to jump *)
    Worker.grow
      { worker with
        chunk=
          Chunk.add_code (Worker.chunk worker)
            [Opcode.JMPNZ (tsize + Worker.size worker + 10)] }
      5
    (* the + 10 inside the JMPNZ is to take care of the JMP operation we'll add
       later *)
  in
  (* the then_ worker now have the code of the then_ + the good header *)
  let worker =
    { worker with
      chunk=
        Chunk.merge_hd
          (* we're in the same time merging symbols from the then to the main
             worker *)
          (Worker.chunk then_)
          (Chunk.add_code (Worker.chunk worker)
             (Chunk.code (Worker.chunk then_)) ) }
  in
  Printf.printf "tsize: %d\n" tsize ;
  let worker = Worker.grow worker tsize in
  let else_ =
    compile_statement else_
      {worker with chunk= Chunk.set (Worker.chunk worker) empty; size= 0}
  in
  let esize = Worker.size else_ in
  Printf.printf "esize: %d\n" esize ;
  Printf.printf "Worker size: %d\n" (Worker.size worker) ;
  let worker =
    { worker with
      chunk=
        Chunk.add_code (Worker.chunk worker)
          [Opcode.JMP (Worker.size worker + 5 + esize)]
        (* here, we're not adding + 10 since we already did inside tsize for the
           JMPNZ *) }
  in
  let worker =
    Worker.grow
      { worker with
        chunk=
          Chunk.merge_hd
            (* we're in the same time merging symbols from the then to the main
               worker *)
            (Worker.chunk else_)
            (Chunk.add_code (Worker.chunk worker)
               (Chunk.code (Worker.chunk else_)) ) }
      (Worker.size else_ + 5)
  in
  Printf.printf "Final size: %de\n" (Worker.size worker) ;
  ignore (Code.emit (Code.set worker Code.empty)) ;
  worker

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
    (stmts : statement list) (nargs : int) (worker : Worker.t) : Worker.t =
  let worker =
    {worker with chunk= Chunk.set (Worker.chunk worker) Opcode.empty; size= 0}
  in
  let chunk, key =
    Chunk.add (Worker.chunk worker) (V_Function (Int32.of_int (unique_id ())))
  in
  let chunk, key = Chunk.addk chunk (V_Variable id) key in
  let worker =
    Worker.addjmp {worker with chunk} (Base.identificator_to_string id) key
  in
  let rec get_locals (largs : int) (worker : Worker.t) = function
    | [] ->
        worker
    | var :: t ->
        let chunk, key = Chunk.add (Worker.chunk worker) (V_Variable var) in
        let value = if largs = 0 then Opcode.LDVOID else POP in
        get_locals (largs - 1)
          (Worker.grow
             {worker with chunk= Chunk.add_code chunk [Opcode.EXTEND key; value]}
             6 )
          t
  in
  let worker =
    Worker.grow
      {worker with chunk= Chunk.add_code (Worker.chunk worker) [Opcode.PUSHENV]}
      1
  in
  let worker = get_locals nargs worker (List.rev vars) in
  let worker = compile_stmt_list stmts worker in
  let opcodes =
    Opcode.add_list Opcode.empty [Opcode.RETURN 0; Opcode.POPENV; PUSH; LDVOID]
  in
  let chunk = Worker.chunk worker in
  let worker =
    Worker.grow {worker with chunk= Chunk.add_code chunk opcodes} 5
  in
  {worker with chunk= Chunk.emplace (Worker.chunk worker) true}

and compile_statement (stmt : statement) (worker : Worker.t) : Worker.t =
  match stmt with
  | Return (_, expr) ->
      compile_return expr 1 worker
  | Block (_, block) ->
      compile_block block worker
  | Assign (_, var, expr) ->
      compile_assign var expr worker
  | If (_, cond, then_, else_) ->
      compile_if cond then_ else_ worker
  | Match (_, pattern, cases) ->
      compile_match pattern cases worker
  | FuncDef (_, id, vars, stmt) ->
      compile_function id vars stmt (List.length vars) worker
  | Expression (_, expr, _) ->
      compile_expr expr worker
  | _ ->
      worker

let adjust_jumps (code : Opcode.t) (pos : int) : Opcode.t =
  let rec aux acc = function
    | [] ->
        List.rev acc
    | Opcode.JMPNZ i :: t ->
        aux (Opcode.JMPNZ (i + pos) :: acc) t
    | Opcode.JMPZ i :: t ->
        aux (Opcode.JMPZ (i + pos) :: acc) t
    | Opcode.JMP i :: t ->
        aux (Opcode.JMP (i + pos) :: acc) t
    | h :: t ->
        aux (h :: acc) t
  in
  aux [] code

let compute_size (code : Opcode.t) : int =
  let rec aux acc = function
    | [] ->
        acc
    | h :: t ->
        aux (acc + Opcode.size h) t
  in
  aux 0 code

let resolve_addresses (code : Code.t) : Code.t =
  let main, rest = code in
  let main = Option.value main ~default:Worker.empty in
  let sizes =
    let rec aux acc = function
      | [] ->
          List.rev acc
      | worker :: t ->
          let size = compute_size (worker |> Worker.chunk |> Chunk.code) in
          aux ((size, Some worker) :: acc) t
    in
    aux [(Worker.size main, None)] rest
  in
  let todo = ref [] in
  let iter (key : int) (value : Base.t) : unit =
    match value with V_Function _ -> todo := key :: !todo | _ -> ()
  in
  let rec aux (code : Code.t) (size : int) =
    let main = Code.get code in
    let main = Option.value main ~default:Worker.empty in
    function
    | [], [] ->
        code
    | [], (dsize, worker) :: t' ->
        let code =
          match worker with
          | Some worker ->
              let ccode = Chunk.code (Worker.chunk worker) in
              let ccode = adjust_jumps ccode size in
              let chunk = Chunk.set (Worker.chunk worker) ccode in
              let worker = {worker with chunk} in
              Code.add worker code
          | None ->
              code
        in
        aux code (dsize + size) ([], t')
    | key :: t, (dsize, worker) :: t' ->
        let nsize = size + dsize in
        let nfunc = Base.V_Function (Int32.of_int nsize) in
        let main =
          {main with chunk= Chunk.setk (Worker.chunk main) key nfunc}
        in
        let code =
          match worker with
          | Some worker ->
              let ccode = Chunk.code (Worker.chunk worker) in
              let ccode = adjust_jumps ccode size in
              let chunk = Chunk.set (Worker.chunk worker) ccode in
              let worker = {worker with chunk} in
              Code.add worker code
          | None ->
              code
        in
        aux (Code.set main code) nsize (t, t')
    | _ ->
        code
  in
  Chunk.iter (Worker.chunk main) iter ;
  let code = aux (Code.set main Code.empty) 0 (List.rev !todo, sizes) in
  code

let compile (ast : program) : Bytes.t =
  let code = Code.empty in
  let rec aux (ast : program) (code : Code.t) : Code.t =
    match ast with
    | [] ->
        code
    | stmt :: t ->
        let main = Option.value (Code.get code) ~default:Worker.empty in
        Printf.printf "Main worker size: %d\n" (Worker.size main) ;
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
          let code = Code.add worker code in
          let code = Code.set main code in
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
  let worker = Option.value (Code.get code) ~default:Worker.empty in
  Chunk.dump (Worker.chunk worker) ;
  let bytes = Code.emit code in
  bytes
