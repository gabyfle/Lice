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
    [acc]: The accumulator of our stack machine, accepting [Base.t] elements
    [stack]: The actual stack of our stack machine
    [rstack]: The return stack of our stack machine
    [pc]: the pointer towards program instructions *)
type 'a t =
  {acc: 'a; stack: 'a list list; rstack: int Stack.t; flag: int; pc: int}

let init_cpu acc = {acc; stack= [[]]; rstack= Stack.create (); flag= -1; pc= 0}

let acc cpu = cpu.acc

let stack cpu = cpu.stack

let push cpu =
  let stack =
    match cpu.stack with
    | [] ->
        [[acc cpu]]
    | h :: t -> (
      match h with [] -> [acc cpu] :: t | _ :: _ as l -> (acc cpu :: l) :: t )
  in
  {cpu with stack}

let pop cpu =
  let sstack = cpu.stack in
  let acc, stack =
    match sstack with
    | [] ->
        raise Stack.Empty
    | stack :: t -> (
      match stack with [] -> raise Stack.Empty | h :: t' -> (h, t' :: t) )
  in
  {cpu with acc; stack}

let push_stack cpu n =
  let values, cpu =
    let rec get_vals (n : int) (acc : 'a list) (cpu : 'a t) =
      match n with
      | 0 ->
          (acc, cpu)
      | k -> (
          let st = List.hd cpu.stack in
          match st with
          | [] ->
              get_vals (k - 1) acc cpu
          | h :: t ->
              get_vals (k - 1) (h :: acc)
                {cpu with stack= t :: List.tl cpu.stack} )
    in
    get_vals n [] cpu
  in
  let stack =
    match cpu.stack with [] -> [[]] | h :: t -> List.rev values :: h :: t
  in
  {cpu with stack}

let pop_stack cpu =
  let stack = match cpu.stack with [] -> raise Stack.Empty | _ :: t -> t in
  {cpu with stack}

let rpush cpu =
  Stack.push cpu.pc cpu.rstack ;
  cpu

let rpop cpu =
  let pc = Stack.pop cpu.rstack in
  {cpu with pc}

let get_flag cpu = cpu.flag

let set_flag cpu flag = {cpu with flag}

let get_pc cpu = cpu.pc

let set_pc cpu pc = {cpu with pc}

let add_pc cpu amount = {cpu with pc= cpu.pc + amount}

let get_acc cpu = cpu.acc

let set_acc cpu acc = {cpu with acc}
