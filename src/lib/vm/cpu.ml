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

(**
    acc: The accumulator of our stack machine, accepting Base.t elements
    stack: The actual stack of our stack machine
    rstack: The return stack of our stack machine
    pc: the pointer towards program instructions *)
type 'a t = {acc: 'a; stack: 'a Stack.t; rstack: int Stack.t; flag: int; pc: int}

let init_cpu acc =
  {acc; stack= Stack.create (); rstack= Stack.create (); flag= 0; pc= 0}

let push cpu =
  Stack.push cpu.acc cpu.stack ;
  cpu

let pop cpu =
  let acc = Stack.pop cpu.stack in
  {cpu with acc}

let get_flag cpu = cpu.flag

let set_flag cpu flag = {cpu with flag}

let get_pc cpu = cpu.pc

let set_pc cpu pc = {cpu with pc}

let add_pc cpu amount = {cpu with pc= cpu.pc + amount}

let get_acc cpu = cpu.acc

let set_acc cpu acc = {cpu with acc}
