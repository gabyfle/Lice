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
  | ADD of int64 * int64
  | SUB of int64 * int64
  | MUL of int64 * int64
  | DIV of int64 * int64
  | MOD of int64 * int64
  | NEG of int64 * int64
  (* Comparision operators *)
  | LT of int64 * int64
  | GT of int64 * int64
  | LE of int64 * int64
  | GE of int64 * int64
  | NE of int64 * int64
  | EQ of int64 * int64
  (* Logical operators *)
  | AND of int64 * int64 * int64
  | OR of int64 * int64 * int64
  | NOT of int64 * int64
  (* Memory operators *)
  | LOADI of int64 * Base.value
  | LOAD of int64 * int64
  | STORE of int64 * int64
  | MOVE of int64 * int64
  (* Control flow operators *)
  | JMP of int64
  | GOTO of int64
  | CALL of int64
  | RET
  (* Stack operators *)
  | SCP_DUPLICATE
  | SCP_CLEAR
  | PUSH of int64
  | POP of int64

type t = opcode list

val empty : t

val pp : Format.formatter -> t -> unit
