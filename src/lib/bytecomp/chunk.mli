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

open Types

type t

val empty : t
(**
    The empty chunk value. This is the base to create a new chunk *)

val add : t -> Base.t -> t
(**
    [add chunk symbol] adds a symbol to the chunk at the end of chunk *)

val set : t -> Opcode.t -> t
(**
    [set chunk code] sets the code of the chunk *)

val get : t -> int -> Base.t
(**
    [get chunk index] returns the symbol at index *)

val load : t -> Bytes.t -> t
(**
    [load chunk bytes] loads the bytecode into the chunk.
    This function also parse the header provided inside the
    bytecode and separate the header from the code *)

val iter : t -> (int -> Base.t -> unit) -> unit
(**
    [iter chunk f] iterates over the symbols of the chunk *)

val emit : t -> bytes
(**
    [emit chunk] emits the given chunk into a string of bytes *)

val reader : Bytes.t -> t * (int -> Opcode.opcode * int)
(**
    [reader bytes] construct a reader function over a byte string *)

val code : t -> Opcode.t
(**
    [code chunk] returns the opcodes of the chunk *)

val bytecode : t -> Bytes.t
(**
    [bytecode chunk] returns the bytecode of the chunk *)
