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

val dump : t -> unit
(**
    [dump chunk] prints the chunk to the standard output *)

val copy_hd : t -> t -> t
(**
    [copy_hd chunk chunk'] copies header of [chunk] into [chunk'] *)

val emplace : t -> bool -> t
(**
    [emplace chunk is_emplace] creates a new chunk with the emplace flag *)

val emplaced : t -> bool
(**
    [emplaced chunk] returns the emplace flag of the chunk *)

val add : t -> Base.t -> t * int
(**
    [add chunk symbol] adds a symbol to the chunk at the end of chunk, returns also the key index where it has been added *)

val removei : t -> Base.t -> t
(**
    [removei chunk index] removes the symbol at index *)

val addk : t -> Base.t -> int -> t * int

val setk : t -> int -> Base.t -> t

val merge_hd : t -> t -> t
(**
    [merge_hd chunk chunk'] merges the header of [chunk] into [chunk'] *)

val add_code : t -> Opcode.t -> t
(**
    [add_code chunk code] adds a code to the chunk at the end of chunk *)

val set : t -> Opcode.t -> t
(**
    [set chunk code] sets the code of the chunk *)

val get : t -> int -> Base.t
(**
    [get chunk index] returns the symbol at index *)

val get_key : t -> Base.t -> int option
(**
    [get_key chunk value] returns the symbol at index *)

val length : t -> int
(**
    [length chunk] returns the number of symbols in the chunk *)

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

val emit_code : t -> bytes
(**
    [emit_code chunk] emits the code of the chunk into a string of bytes *)

val reader : Bytes.t -> t * (int -> Opcode.opcode * int)
(**
    [reader bytes] construct a reader function over a byte string *)

val code : t -> Opcode.t
(**
    [code chunk] returns the opcodes of the chunk *)

val bytecode : t -> Bytes.t
(**
    [bytecode chunk] returns the bytecode of the chunk *)
