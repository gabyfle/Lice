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

open Env
open Types
open Bytecomp

type t = {cpu: Base.t Cpu.t; memory: Environment.t; code: Bytes.t}

let create () =
  let cpu = Cpu.init_cpu Base.V_Void in
  let memory = Environment.empty in
  {cpu; memory; code= Bytes.empty}

let load t code = {t with code}

let cpu t = t.cpu

let memory t = t.memory

let code t = t.code

let pc t = Cpu.get_pc t.cpu

let read t =
  let code = code t in
  let start = pc t in
  let opcode, size = Opcode.of_bytes code start in
  let cpu = Cpu.add_pc (cpu t) size in
  (opcode, {t with cpu})

let nop (t : t) = t

let halt (t : t) = t

let ldbool (t : t) (v : bool) = Cpu.set_acc t.cpu (V_Boolean (Lbool.from v))

let add (t : t) =
  let a = Cpu.get_acc t.cpu in
  let b = Cpu.get_acc (Cpu.pop t.cpu) in
  let res = Value.add a b in
  Cpu.set_acc t.cpu res

let sub (t : t) =
  let a = Cpu.get_acc t.cpu in
  let b = Cpu.get_acc (Cpu.pop t.cpu) in
  let res = Value.sub a b in
  Cpu.set_acc t.cpu res

let mul (t : t) =
  let a = Cpu.get_acc t.cpu in
  let b = Cpu.get_acc (Cpu.pop t.cpu) in
  let res = Value.mul a b in
  Cpu.set_acc t.cpu res

let div (t : t) =
  let a = Cpu.get_acc t.cpu in
  let b = Cpu.get_acc (Cpu.pop t.cpu) in
  let res = Value.div a b in
  Cpu.set_acc t.cpu res

let md (t : t) =
  let a = Cpu.get_acc t.cpu in
  let b = Cpu.get_acc (Cpu.pop t.cpu) in
  let res = Value.md a b in
  Cpu.set_acc t.cpu res

let eq (t : t) =
  let a = Cpu.get_acc t.cpu in
  let b = Cpu.get_acc (Cpu.pop t.cpu) in
  ()

let jmp t d =
  let cpu = Cpu.set_pc t.cpu d in
  {t with cpu}

let jmpnz t d =
  let cpu = if Cpu.get_flag t.cpu <> 0 then Cpu.set_pc t.cpu d else t.cpu in
  {t with cpu}

let jmpz t d =
  let cpu = if Cpu.get_flag t.cpu = 0 then Cpu.set_pc t.cpu d else t.cpu in
  {t with cpu}

let push t =
  let cpu = Cpu.push (cpu t) in
  {t with cpu}

let pop t =
  let cpu = Cpu.pop (cpu t) in
  {t with cpu}

let extend t id =
  let v = Cpu.get_acc t.cpu in
  let memory = Environment.set_var t.memory id v in
  {t with memory}

let search t id =
  let v = Environment.get_var t.memory id in
  match v with
  | None ->
      failwith "Variable not found"
      (* TODO: Handle this kind of error using Located_errors.mli *)
  | Some v ->
      let cpu = Cpu.set_acc t.cpu v in
      {t with cpu}

let pushenv t =
  let memory = Environment.push_scope t.memory in
  {t with memory}

let popenv t =
  let memory = Environment.pop_scope t.memory in
  {t with memory}

let do_code t =
  let rec aux (vm : t) =
    let opcode, vm = read vm in
    Opcode.pp Format.std_formatter opcode ;
    Format.force_newline () ;
    if opcode = Opcode.HALT then vm else aux vm
  in
  aux t
