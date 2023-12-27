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

type registers =
  {mutable r0: int64; mutable r1: int64; mutable r2: int64; mutable r3: int64}

type opcodes =
  (* Arithmetic operators *)
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  (* Comparison operators *)
  | EQ
  | NEQ
  | LT
  | GT
  | LTE
  | GTE
  (* Logical operators *)
  | AND
  | OR
  | NOT
  (* Modules and functions operators *)
  | CALL
  | RET
  (* Memory operators *)
  | LOAD
  | STORE

  (* Modules *)
  | OPEN
  | MODULE
