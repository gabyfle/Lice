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
  | NOP
  | HALT
  | NUMBER of float (* creates a float constant *)
  | STRING of string (* creates a string constant *)
  | LOADK of int (* loads the nth constant into the acc *)
  | LDBOL of bool
  (* Binary operator *)
  | BIN of int
  (* Comparison operator *)
  | CMP of int
  | JMP of int * int
  (* (t, d) Jump to instruction adress d if flag register statisfy t *)
  (* Memory operators *)
  | PUSH (* Push the accumulateur content into the stack *)
  | POP (* Pop the stack into the accumulateur *)
  | EXTEND of string (* Extend the environnement with ENV[X] = V *)
  | SEARCH of
      string (* Search for the value of the variable in the environnement *)
  | CALL of int (* Call the function from the accumulator *)
  | RETURN (* Return from the function *)

type t = opcode list

let empty = []

let add code (opcode : opcode) = opcode :: code

let add_list code (opcodes : opcode list) = List.rev_append opcodes code

let string_to_bytes s =
  let bytes = Bytes.create (String.length s) in
  for i = 0 to String.length s - 1 do
    Bytes.set bytes i s.[i]
  done ;
  bytes

let float_to_bytes f =
  let bytes = Bytes.create 8 in
  let f = ref (Int64.bits_of_float f) in
  for i = 0 to 7 do
    Bytes.set bytes i (Char.chr (Int64.to_int (Int64.logand !f 0xFFL))) ;
    f := Int64.shift_right !f 8
  done ;
  bytes

let emit_byte = function
  | NOP ->
      Bytes.create 1
  | HALT ->
      Bytes.create 1
  | NUMBER f ->
      (* we're encoding floats into a 64-bits *)
      let float = float_to_bytes f in
      let bytes = Bytes.create 9 in
      Bytes.set bytes 0 '\x02' ;
      Bytes.blit float 0 bytes 1 8 ;
      bytes
  | STRING s ->
      let bytes = Bytes.create (String.length s + 1) in
      Bytes.set bytes 0 '\x03' ;
      Bytes.blit (string_to_bytes s) 0 bytes 1 (String.length s) ;
      bytes
  | LDBOL b ->
      let bool = if b then '\x01' else '\x00' in
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 '\x03' ; Bytes.set bytes 1 bool ; bytes
  | BIN n ->
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 '\x04' ;
      Bytes.set bytes 2 (Char.chr n) ;
      bytes
  | JMP (t, d) ->
      let bytes = Bytes.create 6 in
      Bytes.set bytes 0 '\x05' ;
      Bytes.set bytes 1 (Char.chr t) ;
      (* code adresses are hold inside 32-bits values this limit the total code
         size for a single file to 4294967296 *)
      bytes
  | _ ->
      Bytes.create 1

let emit_bytes opcodes =
  let bytes = Bytes.create (List.length opcodes) in
  let rec aux (bytes : Bytes.t) = function [] -> bytes | _ -> bytes in
  ()

let pp (ppf : Format.formatter) (code : t) =
  let pp_opcode ppf = function
    | NOP ->
        Format.fprintf ppf "NOP"
    | HALT ->
        Format.fprintf ppf "HALT"
    | LDNUM f ->
        Format.fprintf ppf "LDNUM %f" f
    | LDBOL b ->
        Format.fprintf ppf "LDBOL %b" b
    | LDSTR s ->
        Format.fprintf ppf "LDSTR %s" s
    | BIN n ->
        Format.fprintf ppf "BIN %d" n
    | JMP (t, d) ->
        Format.fprintf ppf "JMP %d %d" t d
    | CMP n ->
        Format.fprintf ppf "CMP %d" n
    | PUSH ->
        Format.fprintf ppf "PUSH"
    | POP ->
        Format.fprintf ppf "POP"
    | EXTEND v ->
        Format.fprintf ppf "EXTEND %s" v
    | SEARCH v ->
        Format.fprintf ppf "SEARCH %s" v
    | CALL n ->
        Format.fprintf ppf "CALL %d" n
    | RETURN ->
        Format.fprintf ppf "RETURN"
  in
  Format.fprintf ppf "@[<v 2>@[<v 2>Generated code:@,%a@]@,@]"
    (Format.pp_print_list pp_opcode)
    code
