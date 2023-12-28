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

let get registers = function
  | 0 ->
      registers.r0
  | 1 ->
      registers.r1
  | 2 ->
      registers.r2
  | 3 ->
      registers.r3
  | _ ->
      failwith "Not a valid register"

let set registers r v =
  match r with
  | 0 ->
      {registers with r0= v}
  | 1 ->
      {registers with r1= v}
  | 2 ->
      {registers with r2= v}
  | 3 ->
      {registers with r3= v}
  | _ ->
      failwith "Not a valid register"

let arithmetic registers = function
  | ADD (dest, r, r') ->
      set registers dest (Value.add (get registers r) (get registers r'))
  | SUB (dest, r, r') ->
      set registers dest (Value.sub (get registers r) (get registers r'))
  | MUL (dest, r, r') ->
      set registers dest (Value.mul (get registers r) (get registers r'))
  | DIV (dest, r, r') ->
      set registers dest (Value.div (get registers r) (get registers r'))
  | MOD (dest, r, r') ->
      set registers dest (Value.md (get registers r) (get registers r'))
  | _ ->
      failwith "Not an arithmetic operator"

let comparison registers = function
  | EQ (dest, r, r') ->
      set registers dest (Value.eq (get registers r) (get registers r'))
  | NEQ (dest, r, r') ->
      set registers dest (Value.neq (get registers r) (get registers r'))
  | LT (dest, r, r') ->
      let cmp = Value.compare (get registers r) (get registers r') < 0 in
      set registers dest (V_Boolean cmp)
  | GT (dest, r, r') ->
      let cmp = Value.compare (get registers r) (get registers r') > 0 in
      set registers dest (V_Boolean cmp)
  | LTE (dest, r, r') ->
      let cmp = Value.compare (get registers r) (get registers r') <= 0 in
      set registers dest (V_Boolean cmp)
  | GTE (dest, r, r') ->
      let cmp = Value.compare (get registers r) (get registers r') >= 0 in
      set registers dest (V_Boolean cmp)
  | _ ->
      failwith "Not a comparison operator"

let logical registers = function
  | AND (dest, r, r') ->
      set registers dest (Value.band (get registers r) (get registers r'))
  | OR (dest, r, r') ->
      set registers dest (Value.bor (get registers r) (get registers r'))
  | NOT (dest, r) ->
      set registers dest (Value.neg (get registers r))
  | XOR (dest, r, r') ->
      set registers dest (Value.bxor (get registers r) (get registers r'))
  | _ ->
      failwith "Not a logical operator"
