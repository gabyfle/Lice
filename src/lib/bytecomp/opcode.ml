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

type opcode =
  | NOP
  | HALT
  | LDVOID (* loads Void value into the accumulator *)
  | LOADK of int (* loads the nth constant into the acc *)
  | LOADV of int (* loads a variable of id id *)
  | LDBOL of bool
  (* Binary operator *)
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  (* Comparison operator *)
  | EQ
  | NEQ
  | LT
  | GT
  | LE
  | GE
  | JMP of int
  (* JMP(d) Jump to instruction address d *)
  | JMPNZ of int
  (* Jump to d if FLAG is non-zero *)
  | JMPZ of int
  (* Jump to d if FLAG is zero *)
  (* Memory operators *)
  | PUSH (* Push the accumulateur content into the stack *)
  | POP (* Pop the stack into the accumulateur *)
  | EXTEND of int (* Extend the environnement with ENV[X] = V *)
  | SEARCH of int (* ACC = ENV[X] *)
  | PUSHENV (* Pushes a new scope frame into the environnement *)
  | POPENV (* Pop the current scope frame from the environnement *)
  (* Function operators *)
  | CALL of int (* Call the function from the accumulator *)
  | RETURN of int (* Return from the function *)

type t = opcode list

let empty = []

let add code (opcode : opcode) = opcode :: code

let add_list code (opcodes : opcode list) = List.append opcodes code

let _string_to_bytes s =
  let bytes = Bytes.create (String.length s) in
  for i = 0 to String.length s - 1 do
    Bytes.set bytes i s.[i]
  done ;
  bytes

let int_to_bytes n =
  let bytes = Bytes.create 4 in
  Bytes.set_int32_be bytes 0 (Int32.of_int n) ;
  bytes

let emit_byte = function
  | NOP ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 0) ;
      bytes
  | HALT ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 1) ;
      bytes
  | LDVOID ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 2) ;
      bytes
  | LOADK k ->
      let id = Bytes.create 1 in
      Bytes.set id 0 (Char.chr 3) ;
      Bytes.cat id (int_to_bytes k)
  | LOADV v ->
      let id = Bytes.create 1 in
      Bytes.set id 0 (Char.chr 4) ;
      Bytes.cat id (int_to_bytes v)
  | LDBOL b ->
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 5) ;
      Bytes.set bytes 1 (Char.chr (if b then 1 else 0)) ;
      bytes
  | ADD ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 6) ;
      bytes
  | SUB ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 7) ;
      bytes
  | MUL ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 8) ;
      bytes
  | DIV ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 9) ;
      bytes
  | MOD ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 10) ;
      bytes
  | EQ ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 11) ;
      bytes
  | NEQ ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 12) ;
      bytes
  | LT ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 13) ;
      bytes
  | GT ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 14) ;
      bytes
  | LE ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 15) ;
      bytes
  | GE ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 16) ;
      bytes
  | JMP d ->
      let id = Bytes.create 1 in
      Bytes.set id 0 (Char.chr 17) ;
      Bytes.cat id (int_to_bytes d)
  | JMPNZ d ->
      let id = Bytes.create 1 in
      Bytes.set id 0 (Char.chr 18) ;
      Bytes.cat id (int_to_bytes d)
  | JMPZ d ->
      let id = Bytes.create 1 in
      Bytes.set id 0 (Char.chr 19) ;
      Bytes.cat id (int_to_bytes d)
  | PUSH ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 20) ;
      bytes
  | POP ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 21) ;
      bytes
  | EXTEND v ->
      let id = Bytes.create 1 in
      Bytes.set id 0 (Char.chr 22) ;
      Bytes.cat id (int_to_bytes v)
  | SEARCH v ->
      let id = Bytes.create 1 in
      Bytes.set id 0 (Char.chr 23) ;
      Bytes.cat id (int_to_bytes v)
  | PUSHENV ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 24) ;
      bytes
  | POPENV ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 25) ;
      bytes
  | CALL n ->
      let id = Bytes.create 1 in
      Bytes.set id 0 (Char.chr 26) ;
      Bytes.cat id (int_to_bytes n)
  | RETURN n ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 27) ;
      Bytes.cat bytes (int_to_bytes n)

let emit_bytes opcodes =
  let rec aux (bytes : Bytes.t) = function
    | [] ->
        bytes
    | opcode :: opcodes ->
        let opcode_bytes = emit_byte opcode in
        let bytes = Bytes.cat bytes opcode_bytes in
        aux bytes opcodes
  in
  aux (Bytes.create 0) opcodes

let emit code = emit_bytes code

let of_bytes (bytes : bytes) (start : int) : opcode * int =
  let v = Bytes.get bytes start in
  match int_of_char v with
  | 0 ->
      (NOP, 1)
  | 1 ->
      (HALT, 1)
  | 2 ->
      (LDVOID, 1)
  | 3 ->
      let k = Bytes.get_int32_be bytes (start + 1) in
      (LOADK (Int32.to_int k), 5)
  | 4 ->
      let v = Bytes.get_int32_be bytes (start + 1) in
      (LOADK (Int32.to_int v), 5)
  | 5 ->
      let v = Bytes.get bytes (start + 1) in
      let b = if v = '1' then true else false in
      (LDBOL b, 2)
  | 6 ->
      (ADD, 1)
  | 7 ->
      (SUB, 1)
  | 8 ->
      (MUL, 1)
  | 9 ->
      (DIV, 1)
  | 10 ->
      (MOD, 1)
  | 11 ->
      (EQ, 1)
  | 12 ->
      (NEQ, 1)
  | 13 ->
      (LT, 1)
  | 14 ->
      (GT, 1)
  | 15 ->
      (LE, 1)
  | 16 ->
      (GE, 1)
  | 17 ->
      let d = Bytes.get_int32_be bytes (start + 1) in
      (JMP (Int32.to_int d), 5)
  | 18 ->
      let d = Bytes.get_int32_be bytes (start + 1) in
      (JMPNZ (Int32.to_int d), 5)
  | 19 ->
      let d = Bytes.get_int32_be bytes (start + 1) in
      (JMPZ (Int32.to_int d), 5)
  | 20 ->
      (PUSH, 1)
  | 21 ->
      (POP, 1)
  | 22 ->
      let v = Bytes.get_int32_be bytes (start + 1) in
      (EXTEND (Int32.to_int v), 5)
  | 23 ->
      let v = Bytes.get_int32_be bytes (start + 1) in
      (SEARCH (Int32.to_int v), 5)
  | 24 ->
      (PUSHENV, 1)
  | 25 ->
      (POPENV, 1)
  | 26 ->
      let n = Bytes.get_int32_be bytes (start + 1) in
      (CALL (Int32.to_int n), 5)
  | 27 ->
      let n = Bytes.get_int32_be bytes (start + 1) in
      (RETURN (Int32.to_int n), 5)
  | _ ->
      (NOP, 1)

let size = function
  | NOP ->
      1
  | HALT ->
      1
  | LDVOID ->
      1
  | LOADK _ ->
      5
  | LOADV _ ->
      5
  | LDBOL _ ->
      2
  | ADD ->
      1
  | SUB ->
      1
  | MUL ->
      1
  | DIV ->
      1
  | MOD ->
      1
  | EQ ->
      1
  | NEQ ->
      1
  | LT ->
      1
  | GT ->
      1
  | LE ->
      1
  | GE ->
      1
  | JMP _ ->
      5
  | JMPNZ _ ->
      5
  | JMPZ _ ->
      5
  | PUSH ->
      1
  | POP ->
      1
  | EXTEND _ ->
      5
  | SEARCH _ ->
      5
  | PUSHENV ->
      1
  | POPENV ->
      1
  | CALL _ ->
      5
  | RETURN _ ->
      5

let pp (ppf : Format.formatter) (opcode : opcode) =
  let pp_opcode ppf = function
    | NOP ->
        Format.fprintf ppf "NOP"
    | HALT ->
        Format.fprintf ppf "HALT"
    | LDVOID ->
        Format.fprintf ppf "LDVOID"
    | LOADK k ->
        Format.fprintf ppf "LOADK %d" k
    | LOADV v ->
        Format.fprintf ppf "LOADV %d" v
    | LDBOL b ->
        Format.fprintf ppf "LDBOL %b" b
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
    | EQ ->
        Format.fprintf ppf "EQ"
    | NEQ ->
        Format.fprintf ppf "NEQ"
    | LT ->
        Format.fprintf ppf "LT"
    | GT ->
        Format.fprintf ppf "GT"
    | LE ->
        Format.fprintf ppf "LE"
    | GE ->
        Format.fprintf ppf "GE"
    | JMP d ->
        Format.fprintf ppf "JMP %d" d
    | JMPNZ d ->
        Format.fprintf ppf "JMPNZ %d" d
    | JMPZ d ->
        Format.fprintf ppf "JMPZ %d" d
    | PUSH ->
        Format.fprintf ppf "PUSH"
    | POP ->
        Format.fprintf ppf "POP"
    | EXTEND v ->
        Format.fprintf ppf "EXTEND %d" v
    | SEARCH v ->
        Format.fprintf ppf "SEARCH %d" v
    | PUSHENV ->
        Format.fprintf ppf "PUSHENV"
    | POPENV ->
        Format.fprintf ppf "POPENV"
    | CALL n ->
        Format.fprintf ppf "CALL %d" n
    | RETURN n ->
        Format.fprintf ppf "RETURN %d" n
  in
  pp_opcode ppf opcode
