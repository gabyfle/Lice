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

(* This is the table that carries all the variables used inside our program and
   their position into the stack *)
module Lookup = struct
  include Map.Make (String)
end

(* This is the inverse map of Lookup. It tracks used positions inside the
   memory. *)
module Memory = struct
  include Map.Make (Int)

  let first_unused (mem : 'a t) : int =
    let rec aux i = if find_opt i mem = None then i else aux (i + 1) in
    aux 0
end

(* This is the map that contains the locations (in byte) of the defined
   functions inside the program *)
module Functions = struct
  include Map.Make (Int)
end

(* This is the state of the program as we run throught the compilation. It
   carries usefull data such as the current bytecode of the program, a lookup
   table that saves stack position of va riables names the next register to
   use *)
module State = struct
  (* Current opcode list, next register and environement lookup table *)
  type t = {code: Opcode.t; reg: int; env: int Lookup.t * bool Memory.t}

  let empty = {code= Opcode.empty; reg= 0; env= (Lookup.empty, Memory.empty)}

  let _reg = snd

  let opcodes t = t.code

  let add_opcode (op : Opcode.opcode) (st : t) : t =
    {st with code= op :: st.code}

  let next_reg (st : t) : t = {st with reg= st.reg + 1}

  let add_opcodes (ops : Opcode.t) (st : t) : t = {st with code= ops @ st.code}

  let add_var (name : Base.identificator) (st : t) : t =
    let pos = Memory.first_unused (snd st.env) in
    let mem = Memory.add pos true (snd st.env) in
    let lookup =
      Lookup.add (Base.identificator_to_string name) pos (fst st.env)
    in
    let env = (lookup, mem) in
    {st with env}

  (* merge_env merges a scope and a deeper scope into one. if a value exists
     inside the deeper scope, then it's not inside the first on *)
  let merge_env (st : t) (st' : t) : t =
    let env = fst st.env in
    let env' = fst st'.env in
    let mem = snd st.env in
    let mem' = snd st'.env in
    let env = Lookup.merge (fun _ _ _ -> None) env env' in
    let mem = Memory.merge (fun _ _ _ -> None) mem mem' in
    {st with env= (env, mem)}

  let op_size (st : t) : int = List.length st.code

  let replace_op (st : t) (nop : Opcode.opcode) (pos : int) : t =
    let rec aux (acc : Opcode.t) (pos : int) = function
      | [] ->
          List.rev acc
      | h :: t ->
          if pos = 0 then List.rev acc @ (nop :: t)
          else aux (h :: acc) (pos - 1) t
    in
    {st with code= aux [] pos st.code}
end

let binop_comp (state : State.t) : Base.binop_type -> Opcode.t =
  let open Base in
  let bin_operator : binary_operator -> Opcode.t = function
    | Plus ->
        [Opcode.ADD (state.reg - 1, state.reg - 2)]
    | Minus ->
        [Opcode.SUB (state.reg - 1, state.reg - 2)]
    | Multiply ->
        [Opcode.MUL (state.reg - 1, state.reg - 2)]
    | Divide ->
        [Opcode.DIV (state.reg - 1, state.reg - 2)]
    | Mod ->
        [Opcode.MOD (state.reg - 1, state.reg - 2)]
  in
  let bin_comparison : binary_comp -> Opcode.t = function
    | Equal ->
        [Opcode.EQ (state.reg - 1, state.reg - 2)]
    | NotEqual ->
        [Opcode.NE (state.reg - 1, state.reg - 2)]
    | Greater ->
        [Opcode.GT (state.reg - 1, state.reg - 2)]
    | GEQ ->
        [Opcode.GE (state.reg - 1, state.reg - 2)]
    | Lesser ->
        [Opcode.LT (state.reg - 1, state.reg - 2)]
    | LEQ ->
        [Opcode.LE (state.reg - 1, state.reg - 2)]
  in
  function
  | `Operator op ->
      bin_operator op
  | `Compare op ->
      bin_comparison op
  | `Cons ->
      [Opcode.HALT]

let const_comp (state : State.t) : Base.value -> State.t =
 fun v -> State.next_reg (State.add_opcode (Opcode.LOADI (state.reg, v)) state)

let var_comp (state : State.t) (var : Base.identificator) : State.t =
  let name = Base.identificator_to_string var in
  let pos = Lookup.find_opt name (fst state.env) in
  match pos with
  | Some pos ->
      State.next_reg (State.add_opcode (Opcode.LOAD (state.reg, pos)) state)
  | None ->
      failwith "Variable not found"

let rec expr_comp (state : State.t) : Base.expr -> State.t =
  let open Base in
  function
  | BinOp (op, left, right) ->
      let left = expr_comp state left in
      let right = expr_comp left right in
      let op = binop_comp right op in
      State.add_opcodes op right
  | Terminal (Const _ as v) ->
      const_comp state v
  | Terminal (V_Var ident) ->
      var_comp state ident
  | _ ->
      state (* TODO: Function call *)

let rec if_comp (state : State.t) (cond : Base.expr) (t : Tree.statement)
    (f : Tree.statement) : State.t =
  let state = expr_comp state cond in
  let tmp = State.add_opcode (Opcode.JMP 0) state in
  let t = stmt_comp tmp t in
  let diff = State.op_size t - State.op_size tmp in
  (* this is used to compute the jump address *)
  let state = State.replace_op t (Opcode.JMP (diff + 1)) diff in
  stmt_comp state f

and match_comp (state : State.t) (expr : Base.expr)
    (cases : (Base.expr * Tree.statement list) list) : State.t =
  let open Tree in
  let state = expr_comp state expr in
  let val_reg = state.reg - 1 in
  let rec aux (state : State.t) = function
    | [] ->
        state
    | (pat, stmts) :: t ->
        let stmts =
          match stmts with
          | [] ->
              []
          | [(Block (_, _) as b)] ->
              [b]
          | h :: t ->
              [Block (get_location h, h :: t)]
          (* we want to be sure that we get a single element with just a block
             inside *)
        in
        let state = expr_comp state pat in
        let pat_reg = state.reg - 1 in
        let state = State.add_opcode (Opcode.EQ (val_reg, pat_reg)) state in
        let tmp = State.add_opcode (Opcode.JMP 0) state in
        let state = List.fold_left stmt_comp tmp stmts in
        let diff = State.op_size state - State.op_size tmp in
        let state = State.replace_op state (Opcode.JMP (diff + 1)) diff in
        aux state t
  in
  aux state cases

and funcdef_comp (state : State.t) (_name : Base.identificator)
    (_params : Base.identificator list) (_block : Tree.statement) : State.t =
  let _st = state in
  (* we're making a copy of the current state *)
  let _nargs = List.length _params in
  state

and stmt_comp (state : State.t) : Tree.statement -> State.t =
  let open Tree in
  function
  | Return (_, e) ->
      expr_comp state e
  | Assign (_, var, e) ->
      let st = expr_comp state e in
      let v = State.add_var var st in
      let var_pos =
        Lookup.find (Base.identificator_to_string var) (fst v.env)
      in
      let store_instr = Opcode.STORE (st.reg - 1, var_pos) in
      State.add_opcode store_instr v
  | Block (_, stmts) ->
      if stmts = [] then state
        (* do not push an useless scope_dup and scope_clear *)
      else
        let dup = State.add_opcode Opcode.SCP_DUPLICATE state in
        let block = List.fold_left stmt_comp dup stmts in
        (* merge the environement of the block with the current one *)
        let res = State.merge_env state block in
        let state = {res with code= block.code} in
        State.add_opcode Opcode.SCP_CLEAR state
  | If (_, expr, t, f) ->
      (* t = true, f = false *)
      if_comp state expr t f
  | Expression (_, e, _) ->
      expr_comp state e
  | Match (_, e, cases) ->
      match_comp state e cases
  | FuncDef (_, name, params, block) ->
      funcdef_comp state name params block
  | _ ->
      State.add_opcode Opcode.HALT state

let bytecomp (prog : Tree.program) : Opcode.t =
  let state = State.empty in
  let rec aux (prog : Tree.program) (state : State.t) : State.t =
    match prog with [] -> state | stmt :: t -> aux t (stmt_comp state stmt)
  in
  List.rev (State.opcodes (aux prog state |> State.add_opcode Opcode.HALT))
