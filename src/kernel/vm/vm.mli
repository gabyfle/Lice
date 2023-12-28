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

type registers = {r0: Base.t; r1: Base.t; r2: Base.t; r3: Base.t}

type opcodes =
  (* Arithmetic operators *)
  | ADD of int * int * int
  | SUB of int * int * int
  | MUL of int * int * int
  | DIV of int * int * int
  | MOD of int * int * int
  (* Comparison operators *)
  | EQ of int * int * int
  | NEQ of int * int * int
  | LT of int * int * int
  | GT of int * int * int
  | LTE of int * int * int
  | GTE of int * int * int
  (* Logical operators *)
  | AND of int * int * int
  | OR of int * int * int
  | NOT of int * int
  | XOR of int * int * int
  (* Modules and functions operators *)
  | CALL
  | RET
  (* Memory operators *)
  | LOAD
  | STORE
  (* Modules *)
  | OPEN
  | MODULE
  | HALT
