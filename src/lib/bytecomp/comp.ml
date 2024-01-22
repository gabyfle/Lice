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
open Env

(* This is the table that carries all the variables / functions used inside our
   program and their position into the stack *)
module Lookup = Scope

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
  type t = {code: Opcode.t; reg: int64; env: Lookup.t * bool Memory.t}

  let empty =
    {code= Opcode.empty; reg= Int64.zero; env= (Lookup.empty, Memory.empty)}

  let reg st = st.reg

  let opcodes t = t.code

  let add_opcode (op : Opcode.opcode) (st : t) : t =
    {st with code= op :: st.code}

  let reset_reg (st : t) : t = {st with reg= Int64.zero}

  let next_reg (st : t) : t =
    let reg = Int64.add st.reg Int64.one in
    if reg > Int64.of_int 15 then reset_reg st
    else {st with reg= Int64.add st.reg Int64.one}

  let add_opcodes (ops : Opcode.t) (st : t) : t = {st with code= ops @ st.code}

  let add_var (name : Base.identificator) (st : t) : t =
    let pos = Memory.first_unused (snd st.env) in
    let mem = Memory.add pos true (snd st.env) in
    let lookup = Lookup.set_var (fst st.env) name (Int64.of_int pos) in
    let env = (lookup, mem) in
    {st with env}

  let deepen_scope (st : t) =
    let mem = snd st.env in
    let env = (Lookup.push_scope (fst st.env), mem) in
    {st with env}

  let raise_scope (st : t) =
    let mem = snd st.env in
    let env = (Lookup.pop_scope (fst st.env), mem) in
    {st with env}

  (* merge_env merges a scope and a deeper scope into one. if a value exists
     inside the deeper scope, then it's not inside the first on *)
  let merge_env (st : t) (st' : t) : t =
    let lookup = fst (raise_scope st').env in
    let mem = snd st.env in
    let mem' = snd st'.env in
    let mem = Memory.merge (fun _ _ _ -> None) mem mem' in
    {st' with env= (lookup, mem)}

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
  let open Int64 in
  let bin_operator : binary_operator -> Opcode.t = function
    | Plus ->
        [Opcode.ADD (sub state.reg one, sub state.reg (of_int 2))]
    | Minus ->
        [Opcode.SUB (sub state.reg one, sub state.reg (of_int 2))]
    | Multiply ->
        [Opcode.MUL (sub state.reg one, sub state.reg (of_int 2))]
    | Divide ->
        [Opcode.DIV (sub state.reg one, sub state.reg (of_int 2))]
    | Mod ->
        [Opcode.MOD (sub state.reg one, sub state.reg (of_int 2))]
  in
  let bin_comparison : binary_comp -> Opcode.t = function
    | Equal ->
        [Opcode.EQ (sub state.reg one, sub state.reg (of_int 2))]
    | NotEqual ->
        [Opcode.NE (sub state.reg one, sub state.reg (of_int 2))]
    | Greater ->
        [Opcode.GT (sub state.reg one, sub state.reg (of_int 2))]
    | GEQ ->
        [Opcode.GE (sub state.reg one, sub state.reg (of_int 2))]
    | Lesser ->
        [Opcode.LT (sub state.reg one, sub state.reg (of_int 2))]
    | LEQ ->
        [Opcode.LE (sub state.reg one, sub state.reg (of_int 2))]
  in
  function
  | `Operator op ->
      bin_operator op
  | `Compare op ->
      bin_comparison op
  | `Cons ->
      [Opcode.HALT]

let const_comp (state : State.t) (value : Base.value) : State.t =
  let next_reg = State.add_opcode (Opcode.LOADI (state.reg, value)) state in
  State.next_reg next_reg

let var_comp (state : State.t) (var : Base.identificator) : State.t =
  let pos = Lookup.get_var (fst state.env) var in
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
  let tmp = State.add_opcode (Opcode.JMP Int64.zero) state in
  let t = stmt_comp tmp t in
  let diff = State.op_size t - State.op_size tmp in
  (* this is used to compute the jump address *)
  let state =
    State.replace_op t
      (Opcode.JMP (Int64.add (Int64.of_int diff) Int64.zero))
      diff
  in
  stmt_comp state f

and match_comp (state : State.t) (expr : Base.expr)
    (cases : (Base.expr * Tree.statement list) list) : State.t =
  let open Tree in
  let state = expr_comp state expr in
  let val_reg = Int64.sub state.reg Int64.one in
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
        let pat_reg = Int64.sub state.reg Int64.one in
        let state = State.add_opcode (Opcode.EQ (val_reg, pat_reg)) state in
        let tmp = State.add_opcode (Opcode.JMP Int64.zero) state in
        let state = List.fold_left stmt_comp tmp stmts in
        let diff = State.op_size state - State.op_size tmp in
        let state =
          State.replace_op state
            (Opcode.JMP (Int64.add (Int64.of_int diff) Int64.zero))
            diff
        in
        aux state t
  in
  aux state cases

and funcdef_comp (state : State.t) (_name : Base.identificator)
    (params : Base.identificator list) (block : Tree.statement) : State.t =
  let block = match block with Block (_, stmts) -> stmts | _ -> [] in
  let dup = State.add_opcode Opcode.SCP_DUPLICATE state in
  let state = State.deepen_scope dup in
  (* we need to populate the state with the arguments passed to the function *)
  let state =
    List.fold_left
      (fun state arg ->
        let state = State.add_var arg state in
        let var_pos = Lookup.get_var (fst state.env) arg in
        match var_pos with
        | None ->
            exit 1
        | Some var_pos ->
            if State.reg state < Int64.of_int 15 then
              let store_instr =
                Opcode.STORE (Int64.sub state.reg Int64.one, var_pos)
              in
              State.add_opcode store_instr state
            else
              (* the remaining parameters are onto the stack *)
              let pop_n_store =
                [Opcode.POP Int64.zero; Opcode.STORE (Int64.zero, var_pos)]
              in
              State.add_opcodes pop_n_store state )
      state params
  in
  let block = List.fold_left stmt_comp state block in
  let res = State.merge_env state block in
  let state = {res with code= block.code} in
  State.add_opcode Opcode.SCP_CLEAR state

and stmt_comp (state : State.t) : Tree.statement -> State.t =
  let open Tree in
  function
  | Return (_, e) ->
      expr_comp state e
  | Assign (_, var, e) -> (
      let st = expr_comp state e in
      let v = State.add_var var st in
      let var_pos = Lookup.get_var (fst v.env) var in
      match var_pos with
      | None ->
          failwith "Variable not found"
      | Some var_pos ->
          let store_instr =
            Opcode.STORE (Int64.sub st.reg Int64.one, var_pos)
          in
          State.add_opcode store_instr v )
  | Block (_, stmts) ->
      if stmts = [] then state
        (* do not push an useless scope_dup and scope_clear *)
      else
        let dup = State.add_opcode Opcode.SCP_DUPLICATE state in
        let state = State.deepen_scope dup in
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
