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
end

let binop_comp (state : State.t) : Base.binop_type -> Opcode.t =
  let open Base in
  let bin_operator : binary_operator -> Opcode.t = function
    | Plus ->
        [Opcode.ADD (state.reg, state.reg - 1)]
    | Minus ->
        [Opcode.SUB (state.reg, state.reg - 1)]
    | Multiply ->
        [Opcode.MUL (state.reg, state.reg - 1)]
    | Divide ->
        [Opcode.DIV (state.reg, state.reg - 1)]
    | Mod ->
        [Opcode.MOD (state.reg, state.reg - 1)]
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
      let op = binop_comp state op in
      State.add_opcodes op right
  | Terminal (Const _ as v) ->
      const_comp state v
  | Terminal (V_Var ident) ->
      var_comp state ident
  | _ ->
      state (* TODO: Function call *)

let rec stmt_comp (state : State.t) : Tree.statement -> State.t =
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
      let store_instr = Opcode.STORE (st.reg, var_pos) in
      State.add_opcode store_instr v
  | Block (_, stmts) ->
      let dup = State.add_opcode Opcode.SCP_DUPLICATE state in
      let block = List.fold_left stmt_comp dup stmts in
      (* merge the environement of the block with the current one *)
      let res = State.merge_env state block in
      let state = {res with code= block.code} in
      State.add_opcode Opcode.SCP_CLEAR state
  | If (_, _, _, _) ->
      State.add_opcode Opcode.HALT state
  | Expression (_, e, _) ->
      expr_comp state e
  | _ ->
      State.add_opcode Opcode.HALT state

let bytecomp (prog : Tree.program) : Opcode.t =
  let state = State.empty in
  let rec aux (prog : Tree.program) (state : State.t) : State.t =
    match prog with [] -> state | stmt :: t -> aux t (stmt_comp state stmt)
  in
  List.rev (State.opcodes (aux prog state |> State.add_opcode Opcode.HALT))
