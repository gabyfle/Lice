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

type opcode =
  | NOP
  | HALT
  | LOADK of int (* loads the nth constant into the acc *)
  | LOADV of int (* loads a variable of id id *)
  | LDBOL of bool
  (* Binary operator *)
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  (* Comparison operator *)
  | EQ
  | NEQ
  | LT
  | GT
  | LE
  | GE
  | JMP of int * int
  (* (t, d) Jump to instruction address d if flag register statisfy t *)
  (* Memory operators *)
  | PUSH (* Push the accumulateur content into the stack *)
  | POP (* Pop the stack into the accumulateur *)
  | EXTEND of int (* Extend the environnement with ENV[X] = V *)
  | SEARCH of int (* ACC = ENV[X] *)
  | CALL of int (* Call the function from the accumulator *)
  | RETURN (* Return from the function *)

type t = opcode list

val empty : t

val add : t -> opcode -> t

val add_list : t -> opcode list -> t

val emit_bytes : t -> Bytes.t

val pp : Format.formatter -> t -> unit
