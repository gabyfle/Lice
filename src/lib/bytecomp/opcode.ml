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
  | JMP of int * int
  (* (t, d) Jump to instruction address d if flag register statisfy t *)
  (* Memory operators *)
  | PUSH (* Push the accumulateur content into the stack *)
  | POP (* Pop the stack into the accumulateur *)
  | EXTEND of int (* Extend the environnement with ENV[X] = V *)
  | SEARCH of int (* ACC = ENV[X] *)
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
  Bytes.set bytes 0 (Char.chr (n land 0xFF)) ;
  Bytes.set bytes 1 (Char.chr ((n lsr 8) land 0xFF)) ;
  Bytes.set bytes 2 (Char.chr ((n lsr 16) land 0xFF)) ;
  Bytes.set bytes 3 (Char.chr ((n lsr 24) land 0xFF)) ;
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
  | JMP (t, d) ->
      let bytes = Bytes.create 9 in
      let t = int_to_bytes t in
      let d = int_to_bytes d in
      Bytes.set bytes 0 (Char.chr 16) ;
      Bytes.blit t 0 bytes 1 4 ;
      Bytes.blit d 0 bytes 5 4 ;
      bytes
  | PUSH ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 17) ;
      bytes
  | POP ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 18) ;
      bytes
  | EXTEND v ->
      let bytes = Bytes.create 5 in
      let id = int_to_bytes v in
      Bytes.set bytes 0 (Char.chr 19) ;
      Bytes.blit id 0 bytes 1 4 ;
      bytes
  | SEARCH v ->
      let bytes = Bytes.create 5 in
      let id = int_to_bytes v in
      Bytes.set bytes 0 (Char.chr 20) ;
      Bytes.blit id 0 bytes 1 4 ;
      bytes
  | CALL n ->
      let bytes = Bytes.create 5 in
      let id = int_to_bytes n in
      Bytes.set bytes 0 (Char.chr 21) ;
      Bytes.blit id 0 bytes 1 4 ;
      bytes
  | RETURN ->
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 (Char.chr 22) ;
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

let pp (ppf : Format.formatter) (code : t) =
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
    | JMP (t, d) ->
        Format.fprintf ppf "JMP %d %d" t d
    | PUSH ->
        Format.fprintf ppf "PUSH"
    | POP ->
        Format.fprintf ppf "POP"
    | EXTEND v ->
        Format.fprintf ppf "EXTEND %d" v
    | SEARCH v ->
        Format.fprintf ppf "SEARCH %d" v
    | CALL n ->
        Format.fprintf ppf "CALL %d" n
    | RETURN ->
        Format.fprintf ppf "RETURN"
  in
  Format.fprintf ppf "@[<v 2>@[<v 2>Generated code:@,%a@]@,@]"
    (Format.pp_print_list pp_opcode)
    code
