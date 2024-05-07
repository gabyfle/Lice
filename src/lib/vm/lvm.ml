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
open Utils.Logger

type t =
  { cpu: Base.t Cpu.t
  ; memory: Environment.t
  ; chunk: Chunk.t
  ; reader: int -> Opcode.opcode * int }

let empty =
  let cpu = Cpu.init_cpu Base.V_Void in
  let memory = Environment.empty in
  {cpu; memory; chunk= Chunk.empty; reader= (fun _ -> (HALT, 0))}

let load t (bytes : Bytes.t) =
  let chunk, reader = Chunk.reader bytes in
  {t with chunk; reader}

let code t = Chunk.bytecode t.chunk

let dump_stack t =
  let stack = Cpu.stack t.cpu in
  let iter_stack (st : Base.t list) =
    Printf.printf "[{\n" ;
    let count = ref 0 in
    let iter_values (v : Base.t) =
      Value.pretty Format.str_formatter v ;
      Logger.debug "%d -> %s\n" !count (Format.flush_str_formatter ()) ;
      count := !count + 1
    in
    List.iter iter_values st ; Logger.debug "\n}]\n"
  in
  List.iter iter_stack stack

let cpu t = t.cpu

let memory t = t.memory

let pc t = Cpu.get_pc t.cpu

let push_callframe (vm : t) (n : int) : t =
  let cpu = Cpu.push_stack vm.cpu n in
  {vm with cpu}

let pop_callframe (vm : t) (n : int) : t =
  let cpu = Cpu.pop_stack vm.cpu n in
  {vm with cpu}

let read t =
  let pc = pc t in
  let opcode, size = t.reader pc in
  let cpu = Cpu.add_pc (cpu t) size in
  (opcode, {t with cpu})

let nop (t : t) = t

let halt (t : t) = Environment.dump t.memory ; t

let ldvoid (t : t) = {t with cpu= Cpu.set_acc t.cpu V_Void}

let loadk (t : t) (k : int) =
  let chunk = t.chunk in
  let v = Chunk.get chunk k in
  {t with cpu= Cpu.set_acc t.cpu v}

let ldbool (t : t) (v : bool) =
  {t with cpu= Cpu.set_acc t.cpu (V_Boolean (Lbool.from v))}

let push t =
  let cpu = Cpu.push (cpu t) in
  {t with cpu}

let pop t =
  try
    let cpu = Cpu.pop (cpu t) in
    {t with cpu}
  with Stack.Empty ->
    let cpu = Cpu.set_acc t.cpu V_Void in
    {t with cpu}

let add (t : t) =
  let a = Cpu.get_acc t.cpu in
  let t = pop t in
  let b = Cpu.get_acc t.cpu in
  let res = Value.add a b in
  {t with cpu= Cpu.set_acc t.cpu res}

let sub (t : t) =
  let a = Cpu.get_acc t.cpu in
  let t = pop t in
  let b = Cpu.get_acc t.cpu in
  let res = Value.sub a b in
  {t with cpu= Cpu.set_acc t.cpu res}

let mul (t : t) =
  let a = Cpu.get_acc t.cpu in
  let t = pop t in
  let b = Cpu.get_acc t.cpu in
  let res = Value.mul a b in
  {t with cpu= Cpu.set_acc t.cpu res}

let div (t : t) =
  let a = Cpu.get_acc t.cpu in
  let t = pop t in
  let b = Cpu.get_acc t.cpu in
  let res = Value.div a b in
  {t with cpu= Cpu.set_acc t.cpu res}

let md (t : t) =
  let a = Cpu.get_acc t.cpu in
  let t = pop t in
  let b = Cpu.get_acc t.cpu in
  let res = Value.md a b in
  {t with cpu= Cpu.set_acc t.cpu res}

let eq (t : t) =
  let a = Cpu.get_acc t.cpu in
  let t = pop t in
  let b = Cpu.get_acc t.cpu in
  if Value.eq a b then {t with cpu= Cpu.set_flag t.cpu 0}
  else {t with cpu= Cpu.set_flag t.cpu 1}

let jmp t d =
  let cpu = Cpu.set_pc t.cpu d in
  {t with cpu}

let jmpnz t d =
  let cpu = if Cpu.get_flag t.cpu <> 0 then Cpu.set_pc t.cpu d else t.cpu in
  {t with cpu}

let jmpz t d =
  let cpu = if Cpu.get_flag t.cpu = 0 then Cpu.set_pc t.cpu d else t.cpu in
  {t with cpu}

let extend t id =
  let v = Cpu.get_acc t.cpu in
  let memory = Environment.set_var t.memory id v in
  {t with memory}

let search t id =
  let v = Environment.get_var t.memory id in
  match v with
  | None ->
      let cpu = Cpu.set_acc t.cpu V_Void in
      {t with cpu}
  | Some v ->
      let cpu = Cpu.set_acc t.cpu v in
      {t with cpu}

let pushenv t =
  let memory = Environment.push_scope t.memory in
  {t with memory}

let popenv t =
  let memory = Environment.pop_scope t.memory in
  {t with memory}

let call t n =
  let cpu = Cpu.rpush t.cpu in
  let acc = Cpu.get_acc cpu in
  let t = push_callframe {t with cpu} n in
  let vm =
    match acc with
    | V_Function f ->
        {t with cpu= Cpu.set_pc t.cpu (Int32.to_int (Lfunction.address f))}
    | V_Variable _ ->
        t
    | _ ->
        t
  in
  vm

let return t (nargs : int) =
  let cpu, address = Cpu.rpop t.cpu in
  let cpu = Cpu.set_pc cpu address in
  let t = pop {t with cpu} in
  let tmp = Cpu.get_acc t.cpu in
  try
    let vm = pop_callframe t nargs in
    {vm with cpu= Cpu.push (Cpu.set_acc vm.cpu tmp)}
  with Stack.Empty -> {t with cpu= Cpu.push cpu}

let do_code t =
  let rec aux (vm : t) =
    let opcode, vm = read vm in
    match opcode with
    | NOP ->
        aux (nop vm)
    | HALT ->
        halt vm
    | LDVOID ->
        aux (ldvoid vm)
    | LOADK k ->
        aux (loadk vm k)
    | LDBOL b ->
        aux (ldbool vm b)
    | ADD ->
        aux (add vm)
    | SUB ->
        aux (sub vm)
    | MUL ->
        aux (mul vm)
    | DIV ->
        aux (div vm)
    | MOD ->
        aux (md vm)
    | EQ ->
        aux (eq vm)
    | JMP d ->
        aux (jmp vm d)
    | JMPNZ d ->
        aux (jmpnz vm d)
    | JMPZ d ->
        aux (jmpz vm d)
    | PUSH ->
        aux (push vm)
    | POP ->
        aux (pop vm)
    | EXTEND n ->
        aux (extend vm n)
    | SEARCH n ->
        aux (search vm n)
    | PUSHENV ->
        aux (pushenv vm)
    | POPENV ->
        aux (popenv vm)
    | CALL n ->
        aux (call vm n)
    | RETURN n ->
        aux (return vm n)
    | _ ->
        halt vm
  in
  aux t
