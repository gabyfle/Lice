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
  | RETURN (* Return from the function *)

type t = opcode list

let empty = []

let add code (opcode : opcode) = opcode :: code

let add_list code (opcodes : opcode list) = List.rev_append opcodes code

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
  | LOADK k ->
      let bytes = Bytes.create 5 in
      let id = int_to_bytes k in
      Bytes.set bytes 0 (Char.chr 2) ;
      Bytes.blit id 0 bytes 1 4 ;
      bytes
  | LOADV v ->
      let bytes = Bytes.create 5 in
      let id = int_to_bytes v in
      Bytes.set bytes 0 (Char.chr 3) ;
      Bytes.blit id 0 bytes 1 4 ;
      bytes
  | LDBOL b ->
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 4) ;
      Bytes.set bytes 1 (Char.chr (if b then 1 else 0)) ;
      bytes
  | ADD ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 5) ;
      bytes
  | SUB ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 6) ;
      bytes
  | MUL ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 7) ;
      bytes
  | DIV ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 8) ;
      bytes
  | MOD ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 9) ;
      bytes
  | EQ ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 10) ;
      bytes
  | NEQ ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 11) ;
      bytes
  | LT ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 12) ;
      bytes
  | GT ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 13) ;
      bytes
  | LE ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 14) ;
      bytes
  | GE ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 15) ;
      bytes
  | JMP d ->
      let bytes = Bytes.create 9 in
      let d = int_to_bytes d in
      Bytes.set bytes 0 (Char.chr 16) ;
      Bytes.blit d 0 bytes 1 4 ;
      bytes
  | JMPNZ d ->
      let bytes = Bytes.create 9 in
      let d = int_to_bytes d in
      Bytes.set bytes 0 (Char.chr 17) ;
      Bytes.blit d 0 bytes 1 4 ;
      bytes
  | JMPZ d ->
      let bytes = Bytes.create 9 in
      let d = int_to_bytes d in
      Bytes.set bytes 0 (Char.chr 18) ;
      Bytes.blit d 0 bytes 1 4 ;
      bytes
  | PUSH ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 19) ;
      bytes
  | POP ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 20) ;
      bytes
  | EXTEND v ->
      let bytes = Bytes.create 5 in
      let id = int_to_bytes v in
      Bytes.set bytes 0 (Char.chr 21) ;
      Bytes.blit id 0 bytes 1 4 ;
      bytes
  | SEARCH v ->
      let bytes = Bytes.create 5 in
      let id = int_to_bytes v in
      Bytes.set bytes 0 (Char.chr 22) ;
      Bytes.blit id 0 bytes 1 4 ;
      bytes
  | PUSHENV ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 23) ;
      bytes
  | POPENV ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 24) ;
      bytes
  | CALL n ->
      let bytes = Bytes.create 5 in
      let id = int_to_bytes n in
      Bytes.set bytes 0 (Char.chr 25) ;
      Bytes.blit id 0 bytes 1 4 ;
      bytes
  | RETURN ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 26) ;
      bytes

let emit_bytes opcodes =
  let bytes = Bytes.create (List.length opcodes) in
  let rec aux (bytes : Bytes.t) (length : int) = function
    | [] ->
        bytes
    | opcode :: opcodes ->
        let opcode_bytes = emit_byte opcode in
        let op_length = Bytes.length opcode_bytes in
        let bytes =
          if Bytes.length bytes <= length + op_length then
            Bytes.extend bytes 0 op_length
          else bytes
        in
        Bytes.blit opcode_bytes 0 bytes length op_length ;
        aux bytes (length + op_length) opcodes
  in
  aux bytes 0 opcodes

let emit code = emit_bytes (List.rev code)

let of_bytes bytes start =
  let v = Bytes.get bytes start in
  match int_of_char v with
  | 0x0 ->
      (NOP, 1)
  | 0x1 ->
      (HALT, 1)
  | 0x2 ->
      let k = Bytes.get_int32_be bytes (start + 1) in
      (LOADK (Int32.to_int k), 5)
  | 0x3 ->
      let v = Bytes.get_int32_be bytes (start + 1) in
      (LOADK (Int32.to_int v), 5)
  | 0x4 ->
      let v = Bytes.get bytes (start + 1) in
      let b = if v = '1' then true else false in
      (LDBOL b, 2)
  | 0x5 ->
      (ADD, 1)
  | 0x6 ->
      (SUB, 1)
  | 0x7 ->
      (MUL, 1)
  | 0x8 ->
      (DIV, 1)
  | 0x9 ->
      (MOD, 1)
  | 0x10 ->
      (EQ, 1)
  | 0x11 ->
      (NEQ, 1)
  | 0x12 ->
      (LT, 1)
  | 0x13 ->
      (GT, 1)
  | 0x14 ->
      (LE, 1)
  | 0x15 ->
      (GE, 1)
  | 0x16 ->
      let d = Bytes.get_int32_be bytes (start + 1) in
      (JMP (Int32.to_int d), 5)
  | 0x17 ->
      let d = Bytes.get_int32_be bytes (start + 1) in
      (JMPNZ (Int32.to_int d), 5)
  | 0x18 ->
      let d = Bytes.get_int32_be bytes (start + 1) in
      (JMPZ (Int32.to_int d), 5)
  | 0x19 ->
      (PUSH, 1)
  | 0x20 ->
      (POP, 1)
  | 0x21 ->
      let v = Bytes.get_int32_be bytes (start + 1) in
      (EXTEND (Int32.to_int v), 5)
  | 0x22 ->
      let v = Bytes.get_int32_be bytes (start + 1) in
      (SEARCH (Int32.to_int v), 5)
  | 0x23 ->
      (PUSHENV, 1)
  | 0x24 ->
      (POPENV, 1)
  | 0x25 ->
      let n = Bytes.get_int32_be bytes (start + 1) in
      (CALL (Int32.to_int n), 5)
  | 0x26 ->
      (RETURN, 1)
  | _ ->
      (NOP, 1)

let pp (ppf : Format.formatter) (opcode : opcode) =
  let pp_opcode ppf = function
    | NOP ->
        Format.fprintf ppf "NOP"
    | HALT ->
        Format.fprintf ppf "HALT"
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
    | RETURN ->
        Format.fprintf ppf "RETURN"
  in
  pp_opcode ppf opcode
