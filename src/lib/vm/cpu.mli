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
type 'a t = {acc: 'a; stack: 'a Stack.t; rstack: int Stack.t; pc: int}

val init_cpu : 'a -> 'a t

val push : 'a t -> 'a -> unit

val pop : 'a t -> 'a

val get_pc : 'a t -> int

val set_pc : 'a t -> int -> 'a t

val add_pc : 'a t -> int -> 'a t

val get_acc : 'a t -> 'a

val set_acc : 'a t -> 'a -> 'a t
