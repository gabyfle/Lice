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

open Ast.Tree
open Types
open Types.Base

let typing_error (loc : Lexing.position) expected actual =
  let char = loc.pos_cnum - loc.pos_bol in
  let line = loc.pos_lnum in
  Format.sprintf "Expected a %s but got a %s at line %d, character %d." expected
    actual line char

let params_number_error (loc : Lexing.position) (fname : string) expected actual
    =
  let char = loc.pos_cnum - loc.pos_bol in
  let line = loc.pos_lnum in
  Format.sprintf
    "Function %s expect %d parameters but called with %d parameter(s) at line \
     %d, character %d."
    fname expected actual line char

let misc_error (loc : Lexing.position) str =
  let char = loc.pos_cnum - loc.pos_bol in
  let line = loc.pos_lnum in
  Format.sprintf "%s. At line %d, character %d." str line char

let expr_format expr =
  let rec aux = function
    | Terminal t -> (
      match t with
      | Const c ->
          Format.asprintf "%a" Value.pretty c
      | V_Var (id, _) ->
          Printf.sprintf "Variable: %s\n" id )
    | BinOp (binop_t, e, e') ->
        let bin =
          match binop_t with
          | `Compare comp ->
              bincomp_to_string comp
          | `Operator op ->
              binop_to_string op
          | `Cons ->
              "::"
        in
        let left = aux e in
        let right = aux e' in
        Printf.sprintf "Binary operator: [%s] %s [%s]\n" left bin right
    | FuncCall (id, expr_list) ->
        let t = ref "" in
        (* bad functionnal code *)
        let iter e = t := !t ^ ";" ^ aux e in
        List.iter iter expr_list ;
        Printf.sprintf "Function call ID: %s; Expression list: %s\n" id !t
  in
  aux expr

let stmt_format stmt =
  let rec aux = function
    | Return (_, e) ->
        Printf.sprintf "Return statement: %s\n\n" (expr_format e)
    | Expression (_, e, t) ->
        Printf.sprintf "Expression statement of type %s: %s\n\n "
          (Value.typ_to_string t) (expr_format e)
    | Block (_, stmt_list) ->
        let t = ref "" in
        (* bad functionnal code *)
        let iter e = t := !t ^ aux e in
        List.iter iter stmt_list ;
        Printf.sprintf "Block statement with statements: %s\n\n" !t
    | Assign (_, (id, _), e) ->
        Printf.sprintf "Assign statement id %s with expression %s\n\n" id
          (expr_format e)
    | FuncDef (_, (name, _), _, def) ->
        let t = aux def in
        Printf.sprintf "Function definition id: %s and definition: %s\n\n" name
          t
    | Match (_, to_match, patterns) ->
        let str_match = expr_format to_match in
        let rec str_patterns acc' = function
          | [] ->
              acc'
          | (p, stmts) :: t ->
              let e = Printf.sprintf "Pattern: %s\n" (expr_format p) in
              let tmp = ref "" in
              let iter s = tmp := !tmp ^ aux s in
              List.iter iter stmts ;
              str_patterns (Printf.sprintf "%s. Statements: %s\n" e !tmp) t
        in
        Printf.sprintf "Matching statement:\n To match: %s\n %s" str_match
          (str_patterns "" patterns)
    | If (_, e, t, f) ->
        Printf.sprintf "If statement on condition %s. If true: %s if false: %s"
          (expr_format e) (aux t) (aux f)
    | ModuleDef (_, name, stmts) ->
        let tmp = ref "" in
        let iter s = tmp := !tmp ^ aux s in
        List.iter iter stmts ;
        Printf.sprintf "Module definition id: %s and definition: %s\n\n" name
          !tmp
  in
  aux stmt

let program_format =
  let rec aux acc = function
    | [] ->
        acc
    | h :: t ->
        aux (acc ^ stmt_format h) t
  in
  aux ""
