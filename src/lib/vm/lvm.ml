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

type t = {cpu: Base.t Cpu.t; memory: Scope.t; code: Bytes.t}

let create () =
  let cpu = Cpu.init_cpu Base.V_Void in
  let memory = Scope.empty in
  {cpu; memory; code= Bytes.empty}

let load t code = {t with code}

let cpu t = t.cpu

let memory t = t.memory

let code t = t.code

let pc t = Cpu.get_pc t.cpu

let read t =
  let code = code t in
  let start = pc t in
  if start >= Bytes.length code then (Opcode.HALT, t)
  else
    let opcode, size = Opcode.of_bytes code start in
    let cpu = Cpu.add_pc (cpu t) size in
    (opcode, {t with cpu})

let do_code t =
  let rec aux (vm : t) =
    let opcode, vm = read vm in
    Opcode.pp Format.std_formatter opcode ;
    if opcode = Opcode.HALT then vm else aux vm
  in
  aux t
