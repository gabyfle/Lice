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
open Utils.Logger

module Integer = struct
  type t = int

  let compare = compare
end

module Table = Map.Make (Integer)

module type SYMBOLS = sig
  type t_symbol =
    | Number of Lnumber.t
    | String of Lstring.t
    | Function of Lfunction.t

  type symbol = Const of t_symbol | Variable of string | None

  type t

  val empty : t

  val add : t -> symbol -> t

  val get : t -> int -> symbol

  (*val remove : t -> int -> t*)

  val iter : (int -> symbol -> unit) -> t -> unit

  val emit : t -> Bytes.t

  val of_bytes : Bytes.t -> int -> symbol * int
end

module Symbols : SYMBOLS = struct
  type t_symbol =
    | Number of Lnumber.t
    | String of Lstring.t
    | Function of Lfunction.t

  type symbol = Const of t_symbol | Variable of string | None

  type t = symbol Table.t

  let empty = Table.empty

  let add (symbols : t) (symbol : symbol) : t =
    Table.add (Table.cardinal symbols) symbol symbols

  let get (symbols : t) (key : int) : symbol =
    Table.find_opt key symbols |> Option.value ~default:None

  (*let remove (symbols : t) (key : int) : t = Table.remove key symbols*)

  let iter (func : int -> symbol -> unit) (symbols : t) : unit =
    let iter (key : int) (value : symbol) = func key value in
    Table.iter iter symbols

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

  let emit_function (func : int32) =
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 3) ;
    Bytes.set_int32_be bytes 1 func ;
    bytes

  let emit_symbol (symbol : symbol) =
    match symbol with
    | Const (Number number) ->
        emit_number (Lnumber.to_float number)
    | Const (String str) ->
        emit_string (Lstring.to_string str)
    | Const (Function func) ->
        emit_function (Lfunction.address func)
    | Variable name ->
        emit_variable name
    | None ->
        Bytes.empty

  let emit (symbols : t) =
    let bytes = ref (Bytes.create 0) in
    let iter (_ : int) (value : symbol) =
      let sym = emit_symbol value in
      bytes := Bytes.cat !bytes sym
    in
    Table.iter iter symbols ; !bytes

  let of_bytes (bytes : Bytes.t) (start : int) =
    let v = Bytes.get bytes start in
    match int_of_char v with
    | 0 ->
        let number = Bytes.get_int32_be bytes (start + 1) in
        let float = Int32.float_of_bits number in
        (Const (Number (Lnumber.from float)), 5)
    | 1 ->
        let length = Bytes.get_int32_be bytes (start + 1) in
        let str = Bytes.sub_string bytes (start + 5) (Int32.to_int length) in
        let str = Lstring.from str in
        (Const (String str), 5 + Int32.to_int length)
    | 2 ->
        let length = Bytes.get_int32_be bytes (start + 1) in
        let str = Bytes.sub_string bytes (start + 5) (Int32.to_int length) in
        (Variable str, 5 + Int32.to_int length)
    | 3 ->
        let func = Bytes.get_int32_be bytes (start + 1) in
        (Const (Function (Lfunction.from func)), 5)
    | _ ->
        (None, 0)
end

module type HEADER = sig
  type t

  val empty : t

  (*val start : t -> int32*)

  val add : t -> Symbols.symbol -> t

  val iter : (int -> Symbols.symbol -> unit) -> t -> unit

  val get : t -> int -> Symbols.symbol

  val emit : t -> Bytes.t

  val of_bytes : bytes -> int -> t * int
end

module Header : HEADER = struct
  type t = {symbols: Symbols.t; start: int32}

  let empty = {symbols= Symbols.empty; start= 0l}

  let set_start (header : t) (start : int32) = {header with start}

  let add (header : t) (symbol : Symbols.symbol) : t =
    let symbols = Symbols.add header.symbols symbol in
    {header with symbols}

  let emit_start (header : t) =
    let start = header.start in
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 6) ;
    Bytes.set_int32_be bytes 1 (Int32.add start 5l) ;
    bytes

  let iter (func : int -> Symbols.symbol -> unit) (header : t) : unit =
    Symbols.iter func header.symbols

  let get (header : t) (key : int) = Symbols.get header.symbols key

  let emit (header : t) =
    let bytes = Bytes.create 0 in
    let symbols = Symbols.emit header.symbols in
    let header = set_start header (Int32.of_int (Bytes.length symbols)) in
    let start = emit_start header in
    let bytes = Bytes.cat bytes symbols in
    let final = Bytes.cat bytes start in
    Bytes.iter (fun c -> Printf.printf "%d " (int_of_char c)) final ;
    final

  let of_bytes (bytes : Bytes.t) (start : int) =
    let rec aux (header : t) (start : int) =
      let symbol, size = Symbols.of_bytes bytes start in
      match symbol with
      | Symbols.None ->
          (* the Symbols.of_bytes found something that is not a symbol *)
          let pc = Bytes.get_int32_be bytes (start + 1) in
          (* it's surelly the start of the code of the header *)
          let header = set_start header pc in
          (header, start + size + 5)
      | _ ->
          let symbols = Symbols.add header.symbols symbol in
          let header = {header with symbols} in
          aux header (start + size)
    in
    aux empty start
end

type t = {header: Header.t; code: Opcode.t; bytecode: Bytes.t}

let empty = {header= Header.empty; code= []; bytecode= Bytes.empty}

let add (chunk : t) (symbol : Base.t) : t =
  match symbol with
  | V_Number number ->
      let symbol = Symbols.Const (Symbols.Number number) in
      {chunk with header= Header.add chunk.header symbol}
  | V_String str ->
      let symbol = Symbols.Const (Symbols.String str) in
      {chunk with header= Header.add chunk.header symbol}
  | V_Function func ->
      let symbol = Symbols.Const (Symbols.Function func) in
      {chunk with header= Header.add chunk.header symbol}
  | V_Variable name ->
      let symbol = Symbols.Variable (Base.identificator_to_string name) in
      {chunk with header= Header.add chunk.header symbol}
  | _ ->
      chunk

let set (chunk : t) (code : Opcode.t) : t = {chunk with code}

let get (chunk : t) (key : int) : Base.t =
  let symbol = Header.get chunk.header key in
  match symbol with
  | Symbols.Const (Symbols.Number number) ->
      Base.V_Number number
  | Symbols.Const (Symbols.String str) ->
      Base.V_String str
  | Symbols.Const (Symbols.Function func) ->
      Base.V_Function func
  | Symbols.Variable name ->
      Base.V_Variable (Base.string_to_identificator name)
  | _ ->
      Base.V_Void

let load (chunk : t) (bytecode : Bytes.t) : t =
  let header, size = Header.of_bytes bytecode 0 in
  let bytecode = Bytes.sub bytecode size (Bytes.length bytecode - size) in
  {chunk with header; bytecode}

let iter (chunk : t) (func : int -> Base.t -> unit) : unit =
  let of_sym (symbol : Symbols.symbol) =
    match symbol with
    | Symbols.Const (Symbols.Number number) ->
        Base.V_Number number
    | Symbols.Const (Symbols.String str) ->
        Base.V_String str
    | Symbols.Const (Symbols.Function func) ->
        Base.V_Function func
    | Symbols.Variable name ->
        Base.V_Variable (Base.string_to_identificator name)
    | _ ->
        Base.V_Void
  in
  let iter (key : int) (symbol : Symbols.symbol) = func key (of_sym symbol) in
  Header.iter iter chunk.header

let emit (chunk : t) : bytes =
  let header = Header.emit chunk.header in
  let code = Opcode.emit chunk.code in
  Bytes.cat header code

let reader (bytes : Bytes.t) =
  let header, size = Header.of_bytes bytes 0 in
  let bytes = Bytes.sub bytes size (Bytes.length bytes - size) in
  let chunk = {empty with header} in
  let func (start : int) =
    let opcode, size = Opcode.of_bytes bytes start in
    Logger.debug "[Reader] %a" Opcode.pp opcode ;
    Format.pp_print_newline Format.std_formatter () ;
    (opcode, size)
  in
  (chunk, func)

let code (chunk : t) = chunk.code

let bytecode (chunk : t) = chunk.bytecode
