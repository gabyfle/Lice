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

type symbols = Number of float | String of string | Variable of string

type func = int32

module Integer = struct
  type t = int

  let compare = compare
end

module Table = Map.Make (Integer)

module Header = struct
  type t =
    { magic: int32
    ; symbols: symbols Table.t
    ; functions: func Table.t
    ; start: int32 }

  let empty =
    {magic= 0l; symbols= Table.empty; functions= Table.empty; start= 0l}

  let set_magic (header : t) (magic : int32) = {header with magic}

  let get_magic (header : t) = header.magic

  let emit_magic (header : t) =
    let magic = header.magic in
    let bytes = Bytes.create 4 in
    Bytes.set_int32_be bytes 0 magic ;
    bytes

  let set_start (header : t) (start : int32) = {header with start}

  let get_start (header : t) = header.start

  let emit_start (header : t) =
    let start = header.start in
    let bytes = Bytes.create 4 in
    Bytes.set_int32_be bytes 0 start ;
    bytes

  let add_symbol (header : t) (symbol : symbols) (key : int) =
    {header with symbols= Table.add key symbol header.symbols}

  let get_symbols (header : t) = header.symbols

  let emit_number (number : float) =
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 0) ;
    let bits = Int32.bits_of_float number in
    Bytes.set_int32_be bytes 1 bits ;
    bytes

  let emit_string (str : string) =
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 1) ;
    let length = Int32.of_int (String.length str) in
    Bytes.set_int32_be bytes 1 length ;
    let str_bytes = Bytes.of_string str in
    Bytes.cat bytes str_bytes

  let emit_variable (name : string) =
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 2) ;
    let length = Int32.of_int (String.length name) in
    Bytes.set_int32_be bytes 1 length ;
    let str_bytes = Bytes.of_string name in
    Bytes.cat bytes str_bytes

  let emit_symbols (header : t) =
    let bytes = ref (Bytes.create 0) in
    let iter (key : int) (value : symbols) =
      let byte_str = Bytes.create 4 in
      Bytes.set_int32_be byte_str 0 (Int32.of_int key) ;
      let sym =
        match value with
        | Number number ->
            emit_number number
        | String str ->
            emit_string str
        | Variable name ->
            emit_variable name
      in
      let byte_str = Bytes.cat byte_str sym in
      bytes := Bytes.cat !bytes byte_str
    in
    Table.iter iter header.symbols ;
    !bytes

  let add_func (header : t) (func : func) (key : int) =
    {header with functions= Table.add key func header.functions}

  let get_functions (header : t) = header.functions
end

type t = {header: Header.t; code: Opcode.t}

let empty = {header= Header.empty; code= []}

let emit (bytecode : t) : bytes =
  let bytes = Bytes.empty in
  let code = Opcode.emit bytecode.code in
  Bytes.cat bytes code
