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

module Integer = struct
  type t = int

  let compare = compare
end

module Table = Map.Make (Integer)

module type SYMBOLS = sig
  type t_symbol = Number of float | String of string | Function of int32

  type symbol = Const of t_symbol | Variable of string | None

  type t

  val empty : t

  val add : t -> int -> symbol -> t

  (*val remove : t -> int -> t val iter : (int -> symbol -> unit) -> t -> unit*)

  val emit : t -> Bytes.t

  val of_bytes : Bytes.t -> int -> symbol * int
end

module Symbols : SYMBOLS = struct
  type t_symbol = Number of float | String of string | Function of int32

  type symbol = Const of t_symbol | Variable of string | None

  type t = symbol Table.t

  let empty = Table.empty

  let add (symbols : t) (key : int) (symbol : symbol) : t =
    Table.add key symbol symbols

  (*let remove (symbols : t) (key : int) : t = Table.remove key symbols*)

  (*let iter (func : int -> symbol -> unit) (symbols : t) : unit = let iter (key
    : int) (value : symbol) = func key value in Table.iter iter symbols*)

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

  let emit_function (func : int32) =
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 3) ;
    Bytes.set_int32_be bytes 1 func ;
    bytes

  let emit_variable (name : string) =
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 2) ;
    let length = Int32.of_int (String.length name) in
    Bytes.set_int32_be bytes 1 length ;
    let str_bytes = Bytes.of_string name in
    Bytes.cat bytes str_bytes

  let emit_symbol (symbol : symbol) =
    match symbol with
    | Const (Number number) ->
        emit_number number
    | Const (String str) ->
        emit_string str
    | Const (Function func) ->
        emit_function func
    | Variable name ->
        emit_variable name
    | None ->
        Bytes.empty

  let emit (symbols : t) =
    let bytes = ref (Bytes.create 0) in
    let iter (key : int) (value : symbol) =
      let byte_str = Bytes.create 4 in
      Bytes.set_int32_be byte_str 0 (Int32.of_int key) ;
      let sym = emit_symbol value in
      let byte_str = Bytes.cat byte_str sym in
      bytes := Bytes.cat !bytes byte_str
    in
    Table.iter iter symbols ; !bytes

  let of_bytes (bytes : Bytes.t) (start : int) =
    let v = Bytes.get bytes start in
    match int_of_char v with
    | 0 ->
        let number = Bytes.get_int32_be bytes (start + 1) in
        (Const (Number (Int32.float_of_bits number)), 5)
    | 1 ->
        let length = Bytes.get_int32_be bytes (start + 1) in
        let str = Bytes.sub_string bytes (start + 5) (Int32.to_int length) in
        (Const (String str), 5 + Int32.to_int length)
    | 2 ->
        let length = Bytes.get_int32_be bytes (start + 1) in
        let str = Bytes.sub_string bytes (start + 5) (Int32.to_int length) in
        (Variable str, 5 + Int32.to_int length)
    | 3 ->
        let func = Bytes.get_int32_be bytes (start + 1) in
        (Const (Function func), 5)
    | _ ->
        (None, 0)
end

module type HEADER = sig
  type t

  val empty : t

  val start : t -> int32

  (*val iter : (int -> Symbols.symbol -> unit) -> Symbols.t -> unit*)

  val emit : t -> Bytes.t

  val of_bytes : bytes -> int -> t * int
end

module Header : HEADER = struct
  type t = {symbols: Symbols.t; start: int32}

  let empty = {symbols= Symbols.empty; start= 0l}

  let set_start (header : t) (start : int32) = {header with start}

  let start (header : t) = header.start

  let emit_start (header : t) =
    let start = header.start in
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 5) ;
    Bytes.set_int32_be bytes 0 start ;
    bytes

  (*let iter (func : int -> Symbols.symbol -> unit) (symbols : Symbols.t) : unit
    = Symbols.iter func symbols*)

  let emit (header : t) =
    let bytes = Bytes.create 0 in
    let symbols = Symbols.emit header.symbols in
    let start = emit_start header in
    Bytes.cat bytes symbols |> Bytes.cat start

  let of_bytes (bytes : Bytes.t) (start : int) =
    let rec aux (header : t) (start : int) =
      let symbol, size = Symbols.of_bytes bytes start in
      match symbol with
      | Symbols.None ->
          (* the Symbols.of_bytes found something that is not a symbol *)
          let start = Bytes.get_int32_be bytes start in
          (* it's surelly the start of the code of the header *)
          let header = set_start header start in
          (header, size)
      | _ ->
          let symbols = Symbols.add header.symbols start symbol in
          let header = {header with symbols} in
          aux header (start + size)
    in
    aux empty start
end

type t = {header: Header.t; code: Opcode.t}

let empty = {header= Header.empty; code= []}

let emit (bytecode : t) : bytes =
  let header = Header.emit bytecode.header in
  let code = Opcode.emit bytecode.code in
  Bytes.cat header code

let reader (bytes : Bytes.t) =
  let header, _ = Header.of_bytes bytes 0 in
  let chunk = {empty with header} in
  let func (start : int) =
    let opcode, size =
      Opcode.of_bytes bytes (Int32.to_int (Header.start header) + start)
    in
    (opcode, size)
  in
  (chunk, func)
