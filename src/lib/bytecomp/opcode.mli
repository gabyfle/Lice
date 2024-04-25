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
    This is the representation of our machine's instructions *)
type opcode =
  | NOP (* does nothing *)
  | HALT (* stops the virtual machine execution *)
  | LDVOID (* loads void into the accumulator *)
  | LOADK of int (* loads the nth constant into the accumulator *)
  | LOADV of int (* loads a variable of id id into the accumulator *)
  | LDBOL of bool (* loads a bool value into the accumulator *)
  (* Binary operator *)
  | ADD (* + *)
  | SUB (* - *)
  | MUL (* * *)
  | DIV (* / *)
  | MOD (* % *)
  (* Comparison operator *)
  | EQ (* == *)
  | NEQ (* != *)
  | LT (* < *)
  | GT (* > *)
  | LE (* <= *)
  | GE (* >= *)
  (* Unconditional and conditional jumps *)
  | JMP of int (* JMP(d) Jump to instruction address d *)
  | JMPNZ of int (* Jump to d if FLAG is non-zero *)
  | JMPZ of int (* Jump to d if FLAG is zero *)
  (* Memory operators *)
  | PUSH (* Push the accumulateur content into the stack *)
  | POP (* Pop the stack into the accumulateur *)
  | EXTEND of int (* Extend the environnement with ENV[X] = ACC *)
  | SEARCH of int (* ACC = ENV[X] *)
  | PUSHENV (* Pushes a new scope frame into the environnement *)
  | POPENV (* Pop the current scope frame from the environnement *)
  (* Function operators *)
  | CALL of int (* Call the function from the accumulator with n parameters *)
  | RETURN (* Return from the function *)

type t = opcode list

val empty : t
(**
    Creates an empty list of opcodes *)

val add : t -> opcode -> t
(**
    [add opcodes opcode] appends [opcode] to the [opcodes] list *)

val add_list : t -> opcode list -> t
(**
    [add_list opcodes opcodes_list] appends the [opcodes_list] list to the [opcodes] list *)

val emit : t -> Bytes.t
(**
    [emit opcodes] returns a [Bytes.t] string representing the bytecode of the [opcodes] list into binary format *)

val size : opcode -> int
(**
    [size opcodes] returns the size of the [opcodes] list in bytes *)

val of_bytes : Bytes.t -> int -> opcode * int
(**
    Reads the next instruction starting from index [start].
    Returns a couple [(opcode, size)] where [opcode] is the read opcode and [size] is the total size of what's been read *)

val pp : Format.formatter -> opcode -> unit
(**
    [pp fmt opcode] pretty prints the [opcode] code with the [fmt] format into the formatter buffer *)
