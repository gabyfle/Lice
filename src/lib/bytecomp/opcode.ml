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
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | NEG
  (* Memory operators *)
  | LDI of Base.value (* Loads Base.value inside the accumulator *)
  | PUSH (* Push the accumulateur content into the stack *)

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
    | ADD ->
        Format.fprintf ppf "ADD"
    | SUB ->
        Format.fprintf ppf "SUB"
    | MUL ->
        Format.fprintf ppf "MUL"
    | DIV ->
        Format.fprintf ppf "DIV"
    | MOD ->
        Format.fprintf ppf "MOD"
    | NEG ->
        Format.fprintf ppf "NEG"
    | LDI v -> (
      match v with
      | Base.Const c ->
          Format.fprintf ppf "LDI %a" Value.pretty c
      | Base.V_Var v ->
          Format.fprintf ppf "LDI %s" (Base.identificator_to_string v) )
    | PUSH ->
        Format.fprintf ppf "PUSH"
  in
  Format.fprintf ppf "@[<v 2>@[<v 2>Generated code:@,%a@]@,@]"
    (Format.pp_print_list pp_opcode)
    code
