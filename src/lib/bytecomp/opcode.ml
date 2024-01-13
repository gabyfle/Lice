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
  | ADD of int * int * int
  | SUB of int * int * int
  | MUL of int * int * int
  | DIV of int * int * int
  | MOD of int * int * int
  | NEG of int * int
  (* Comparision operators *)
  | LT of int * int * int
  | GT of int * int * int
  | LE of int * int * int
  | GE of int * int * int
  | EQ of int * int * int
  | NE of int * int * int
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
  | JZ of int * int
  | JNZ of int * int
  | CALL of int
  | RET
  (* Stack operators *)
  | SCP_DUPLICATE
  | SCP_CLEAR
  | PUSH of int
  | POP of int

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
    | ADD (a, b, c) ->
        Format.fprintf ppf "ADD %d %d %d" a b c
    | SUB (a, b, c) ->
        Format.fprintf ppf "SUB %d %d %d" a b c
    | MUL (a, b, c) ->
        Format.fprintf ppf "MUL %d %d %d" a b c
    | DIV (a, b, c) ->
        Format.fprintf ppf "DIV %d %d %d" a b c
    | MOD (a, b, c) ->
        Format.fprintf ppf "MOD %d %d %d" a b c
    | NEG (a, b) ->
        Format.fprintf ppf "NEG %d %d" a b
    | LT (a, b, c) ->
        Format.fprintf ppf "LT %d %d %d" a b c
    | GT (a, b, c) ->
        Format.fprintf ppf "GT %d %d %d" a b c
    | LE (a, b, c) ->
        Format.fprintf ppf "LE %d %d %d" a b c
    | GE (a, b, c) ->
        Format.fprintf ppf "GE %d %d %d" a b c
    | EQ (a, b, c) ->
        Format.fprintf ppf "EQ %d %d %d" a b c
    | NE (a, b, c) ->
        Format.fprintf ppf "NE %d %d %d" a b c
    | AND (a, b, c) ->
        Format.fprintf ppf "AND %d %d %d" a b c
    | OR (a, b, c) ->
        Format.fprintf ppf "OR %d %d %d" a b c
    | NOT (a, b) ->
        Format.fprintf ppf "NOT %d %d" a b
    | LOADI (a, b) ->
            Format.fprintf ppf "LOADI %d %a" a Base.pretty b
    | LOAD (a, b) ->
        Format.fprintf ppf "LOAD %d %d" a b
    | STORE (a, b) ->
        Format.fprintf ppf "STORE %d %d" a b
    | MOVE (a, b) ->
        Format.fprintf ppf "MOVE %d %d" a b
    | JMP a ->
        Format.fprintf ppf "JMP %d" a
    | JZ (a, b) ->
        Format.fprintf ppf "JZ %d %d" a b
    | JNZ (a, b) ->
        Format.fprintf ppf "JNZ %d %d" a b
    | CALL a ->
        Format.fprintf ppf "CALL %d" a
    | RET ->
        Format.fprintf ppf "RET"
    | SCP_DUPLICATE ->
        Format.fprintf ppf "SCP_DUPLICATE"
    | SCP_CLEAR ->
        Format.fprintf ppf "SCP_CLEAR"
    | PUSH a ->
        Format.fprintf ppf "PUSH %d" a
    | POP a ->
        Format.fprintf ppf "POP %d" a
  in
  Format.fprintf ppf "@[<v 2>@[<v 2>Generated code:@,%a@]@,@]"
    (Format.pp_print_list pp_opcode)
    code
