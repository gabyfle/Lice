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

let empty = []

let pp (ppf : Format.formatter) (code : t) =
  let pp_opcode ppf = function
    | HALT ->
        Format.fprintf ppf "HALT"
    | VALUE v -> (
      match v with
      | Base.Const c ->
          Format.fprintf ppf "VALUE %a" Value.pretty c
      | Base.V_Var v ->
          Format.fprintf ppf "VAR %s" (Base.identificator_to_string v) )
    | ADD (a, b) ->
        Format.fprintf ppf "ADD %Ld %Ld" a b
    | SUB (a, b) ->
        Format.fprintf ppf "SUB %Ld %Ld" a b
    | MUL (a, b) ->
        Format.fprintf ppf "MUL %Ld %Ld" a b
    | DIV (a, b) ->
        Format.fprintf ppf "DIV %Ld %Ld" a b
    | MOD (a, b) ->
        Format.fprintf ppf "MOD %Ld %Ld" a b
    | NEG (a, b) ->
        Format.fprintf ppf "NEG %Ld %Ld" a b
    | LT (a, b) ->
        Format.fprintf ppf "LT %Ld %Ld" a b
    | GT (a, b) ->
        Format.fprintf ppf "GT %Ld %Ld" a b
    | LE (a, b) ->
        Format.fprintf ppf "LE %Ld %Ld" a b
    | GE (a, b) ->
        Format.fprintf ppf "GE %Ld %Ld" a b
    | EQ (a, b) ->
        Format.fprintf ppf "EQ %Ld %Ld" a b
    | NE (a, b) ->
        Format.fprintf ppf "NE %Ld %Ld" a b
    | AND (a, b, c) ->
        Format.fprintf ppf "AND %Ld %Ld %Ld" a b c
    | OR (a, b, c) ->
        Format.fprintf ppf "OR %Ld %Ld %Ld" a b c
    | NOT (a, b) ->
        Format.fprintf ppf "NOT %Ld %Ld" a b
    | LOADI (a, b) -> (
      match b with
      | Base.Const c ->
          Format.fprintf ppf "LOADI %Ld %a" a Value.pretty c
      | Base.V_Var v ->
          Format.fprintf ppf "LOADI %Ld (VAR %s)" a
            (Base.identificator_to_string v) )
    | LOAD (a, b) ->
        Format.fprintf ppf "LOAD %Ld %Ld" a b
    | STORE (a, b) ->
        Format.fprintf ppf "STORE %Ld %Ld" a b
    | MOVE (a, b) ->
        Format.fprintf ppf "MOVE %Ld %Ld" a b
    | JMP a ->
        Format.fprintf ppf "JMP %Ld" a
    | GOTO a ->
        Format.fprintf ppf "GOTO %Ld" a
    | CALL a ->
        Format.fprintf ppf "CALL %Ld" a
    | RET ->
        Format.fprintf ppf "RET"
    | SCP_DUPLICATE ->
        Format.fprintf ppf "SCP_DUPLICATE"
    | SCP_CLEAR ->
        Format.fprintf ppf "SCP_CLEAR"
    | PUSH a ->
        Format.fprintf ppf "PUSH %Ld" a
    | POP a ->
        Format.fprintf ppf "POP %Ld" a
  in
  Format.fprintf ppf "@[<v 2>@[<v 2>Generated code:@,%a@]@,@]"
    (Format.pp_print_list pp_opcode)
    code
