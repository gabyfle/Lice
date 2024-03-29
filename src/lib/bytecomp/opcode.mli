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

type opcode =
  | HALT
  | VALUE of Base.value
  (* Arithmetic operators *)
  | ADD of int * int
  | SUB of int * int
  | MUL of int * int
  | DIV of int * int
  | MOD of int * int
  | NEG of int * int
  (* Comparision operators *)
  | LT of int * int
  | GT of int * int
  | LE of int * int
  | GE of int * int
  | NE of int * int
  | EQ of int * int
  (* Logical operators *)
  | AND of int * int * int
  | OR of int * int * int
  | NOT of int * int
  (* Memory operators *)
  | LOADI of int * Base.value
  | LOAD of int * int
  | STORE of int * int
  | MOVE of int * int
  (* Control flow operators *)
  | JMP of int
  | GOTO of int
  | CALL of int
  | RET
  (* Stack operators *)
  | SCP_DUPLICATE
  | SCP_CLEAR
  | PUSH of int
  | POP of int

type t = opcode list

val empty : t

val pp : Format.formatter -> t -> unit
