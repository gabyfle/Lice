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

module Symbol = struct
  type t_symbol =
    | Number of Lnumber.t
    | String of Lstring.t
    | Function of Lfunction.t

  type t = Const of t_symbol | Variable of string | None

  let compare_vals (a : t_symbol) (b : t_symbol) =
    match (a, b) with
    | Number a, Number b ->
        Lnumber.compare a b
    | String a, String b ->
        Lstring.compare a b
    | Function a, Function b ->
        Lfunction.compare a b
    | _ ->
        -1

  let compare (a : t) (b : t) =
    match (a, b) with
    | Const a, Const b ->
        compare_vals a b
    | Variable a, Variable b ->
        String.compare a b
    | None, None ->
        0
    | _ ->
        -1

  (*let show (symbol : t) = match symbol with | Const (Number number) ->
    Printf.sprintf "Number(%s)" (Lnumber.to_string number) | Const (String str)
    -> Printf.sprintf "String(%s)" (Lstring.to_string str) | Const (Function
    func) -> Printf.sprintf "Function(%s)" (Lfunction.to_string func) | Variable
    name -> Printf.sprintf "Variable(%s)" name | None -> "None"*)
end

module Table = Map.Make (Integer)
module Inverse = Map.Make (Symbol)

module type SYMBOLS = sig
  type t

  val empty : t

  val length : t -> int

  val add : t -> Symbol.t -> t * int

  val addk : t -> Symbol.t -> int -> t * int

  val get : t -> int -> Symbol.t

  val get_key : t -> Symbol.t -> int option

  val iter : (int -> Symbol.t -> unit) -> t -> unit

  val emit : t -> Bytes.t

  val of_bytes : Bytes.t -> int -> Symbol.t * int
end

module Symbols : SYMBOLS = struct
  type t = Symbol.t Table.t * int Inverse.t

  let empty : t = (Table.empty, Inverse.empty)

  let length ((symbols, _) : t) = Table.cardinal symbols

  let add (symbols : t) (symbol : Symbol.t) : t * int =
    let table, inverse = symbols in
    let key = Table.cardinal table in
    let table = Table.add key symbol table in
    let inverse = Inverse.add symbol key inverse in
    ((table, inverse), key)

  let addk (symbols : t) (symbol : Symbol.t) (key : int) : t * int =
    let table, inverse = symbols in
    let key' = Table.cardinal table in
    let table = Table.add key' symbol table in
    let inverse = Inverse.add symbol key inverse in
    ((table, inverse), key')

  let get ((symbols, _) : t) (key : int) : Symbol.t =
    Table.find_opt key symbols |> Option.value ~default:Symbol.None

  let get_key (_, symbols) (symbol : Symbol.t) : int option =
    Inverse.find_opt symbol symbols

  let iter (func : int -> Symbol.t -> unit) ((symbols, _) : t) : unit =
    let iter (key : int) (value : Symbol.t) = func key value in
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

  let emit_symbol (symbol : Symbol.t) =
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

  let emit ((symbols, _) : t) =
    let bytes = ref (Bytes.create 0) in
    let iter (_ : int) (value : Symbol.t) =
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
        (Symbol.Const (Number (Lnumber.from float)), 5)
    | 1 ->
        let length = Bytes.get_int32_be bytes (start + 1) in
        let str = Bytes.sub_string bytes (start + 5) (Int32.to_int length) in
        let str = Lstring.from str in
        (Symbol.Const (String str), 5 + Int32.to_int length)
    | 2 ->
        let length = Bytes.get_int32_be bytes (start + 1) in
        let str = Bytes.sub_string bytes (start + 5) (Int32.to_int length) in
        (Symbol.Variable str, 5 + Int32.to_int length)
    | 3 ->
        let func = Bytes.get_int32_be bytes (start + 1) in
        (Symbol.Const (Function (Lfunction.from func)), 5)
    | _ ->
        (Symbol.None, 0)
end

module type HEADER = sig
  type t

  val empty : t

  (*val start : t -> int32*)

  val add : t -> Symbol.t -> t * int

  val addk : t -> Symbol.t -> int -> t * int

  val length : t -> int

  val iter : (int -> Symbol.t -> unit) -> t -> unit

  val get : t -> int -> Symbol.t

  val get_key : t -> Symbol.t -> int option

  val emit : t -> Bytes.t

  val of_bytes : bytes -> int -> t * int
end

module Header : HEADER = struct
  type t = {symbols: Symbols.t; start: int32}

  let empty = {symbols= Symbols.empty; start= 0l}

  let length (header : t) = Symbols.length header.symbols

  let set_start (header : t) (start : int32) = {header with start}

  let add (header : t) (symbol : Symbol.t) : t * int =
    let symbols, key = Symbols.add header.symbols symbol in
    ({header with symbols}, key)

  let addk (header : t) (symbol : Symbol.t) (key : int) : t * int =
    let symbols, key = Symbols.addk header.symbols symbol key in
    ({header with symbols}, key)

  let emit_start (header : t) =
    let start = header.start in
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 6) ;
    Bytes.set_int32_be bytes 1 (Int32.add start 5l) ;
    bytes

  let iter (func : int -> Symbol.t -> unit) (header : t) : unit =
    Symbols.iter func header.symbols

  let get (header : t) (key : int) = Symbols.get header.symbols key

  let get_key (header : t) (symbol : Symbol.t) =
    Symbols.get_key header.symbols symbol

  let emit (header : t) =
    let bytes = Bytes.create 0 in
    let symbols = Symbols.emit header.symbols in
    let header = set_start header (Int32.of_int (Bytes.length symbols)) in
    let start = emit_start header in
    let bytes = Bytes.cat bytes symbols in
    let final = Bytes.cat bytes start in
    final

  let of_bytes (bytes : Bytes.t) (start : int) =
    let rec aux (header : t) (start : int) =
      let symbol, size = Symbols.of_bytes bytes start in
      match symbol with
      | Symbol.None ->
          (* the Symbols.of_bytes found something that is not a symbol *)
          let pc = Bytes.get_int32_be bytes (start + 1) in
          (* it's surelly the start of the code of the header *)
          let header = set_start header pc in
          (header, start + size + 5)
      | _ ->
          let symbols, _ = Symbols.add header.symbols symbol in
          let header = {header with symbols} in
          aux header (start + size)
    in
    aux empty start
end

type t = {header: Header.t; code: Opcode.t; bytecode: Bytes.t; emplace: bool}

let empty =
  {header= Header.empty; code= []; bytecode= Bytes.empty; emplace= false}

let copy_hd (chunk : t) (chunk' : t) = {chunk' with header= chunk.header}

let emplace (chunk : t) (emplace : bool) : t = {chunk with emplace}

let emplaced (chunk : t) = chunk.emplace

let add (chunk : t) (symbol : Base.t) : t * int =
  match symbol with
  | V_Number number ->
      let symbol = Symbol.Const (Symbol.Number number) in
      let header, key = Header.add chunk.header symbol in
      ({chunk with header}, key)
  | V_String str ->
      let symbol = Symbol.Const (Symbol.String str) in
      let header, key = Header.add chunk.header symbol in
      ({chunk with header}, key)
  | V_Function func ->
      let symbol = Symbol.Const (Symbol.Function func) in
      let header, key = Header.add chunk.header symbol in
      ({chunk with header}, key)
  | V_Variable name ->
      let symbol = Symbol.Variable (Base.identificator_to_string name) in
      let header, key = Header.add chunk.header symbol in
      ({chunk with header}, key)
  | _ ->
      (chunk, -1)

let addk (chunk : t) (symbol : Base.t) (key : int) : t * int =
  match symbol with
  | V_Number number ->
      let symbol = Symbol.Const (Symbol.Number number) in
      let header, key = Header.addk chunk.header symbol key in
      ({chunk with header}, key)
  | V_String str ->
      let symbol = Symbol.Const (Symbol.String str) in
      let header, key = Header.addk chunk.header symbol key in
      ({chunk with header}, key)
  | V_Function func ->
      let symbol = Symbol.Const (Symbol.Function func) in
      let header, key = Header.addk chunk.header symbol key in
      ({chunk with header}, key)
  | V_Variable name ->
      let symbol = Symbol.Variable (Base.identificator_to_string name) in
      let header, key = Header.addk chunk.header symbol key in
      ({chunk with header}, key)
  | _ ->
      (chunk, -1)

let set (chunk : t) (code : Opcode.t) : t = {chunk with code}

let get (chunk : t) (key : int) : Base.t =
  let symbol = Header.get chunk.header key in
  let v =
    match symbol with
    | Symbol.Const (Symbol.Number number) ->
        Base.V_Number number
    | Symbol.Const (Symbol.String str) ->
        Base.V_String str
    | Symbol.Const (Symbol.Function func) ->
        Base.V_Function func
    | Symbol.Variable name ->
        Base.V_Variable (Base.string_to_identificator name)
    | _ ->
        Base.V_Void
  in
  Value.pretty Format.str_formatter v ;
  Logger.error "GOT VALUE %s at KEY %d" (Format.flush_str_formatter ()) key ;
  v

let get_key (chunk : t) (value : Base.t) : int option =
  match value with
  | V_Number number ->
      Header.get_key chunk.header (Symbol.Const (Symbol.Number number))
  | V_String str ->
      Header.get_key chunk.header (Symbol.Const (Symbol.String str))
  | V_Function func ->
      Header.get_key chunk.header (Symbol.Const (Symbol.Function func))
  | V_Variable name ->
      Header.get_key chunk.header
        (Symbol.Variable (Base.identificator_to_string name))
  | _ ->
      None

let length (chunk : t) : int = Header.length chunk.header

let load (chunk : t) (bytecode : Bytes.t) : t =
  let header, size = Header.of_bytes bytecode 0 in
  let bytecode = Bytes.sub bytecode size (Bytes.length bytecode - size) in
  {chunk with header; bytecode}

let iter (chunk : t) (func : int -> Base.t -> unit) : unit =
  let of_sym (symbol : Symbol.t) =
    match symbol with
    | Symbol.Const (Symbol.Number number) ->
        Base.V_Number number
    | Symbol.Const (Symbol.String str) ->
        Base.V_String str
    | Symbol.Const (Symbol.Function func) ->
        Base.V_Function func
    | Symbol.Variable name ->
        Base.V_Variable (Base.string_to_identificator name)
    | _ ->
        Base.V_Void
  in
  let iter (key : int) (symbol : Symbol.t) = func key (of_sym symbol) in
  Header.iter iter chunk.header

let emit (chunk : t) : Bytes.t =
  let header = Header.emit chunk.header in
  Logger.warning "Emitting this bytecode:" ;
  let rec pp_code (code : Opcode.t) : unit =
    match code with
    | [] ->
        ()
    | opcode :: rest ->
        Opcode.pp Format.str_formatter opcode ;
        Logger.warning "%s" (Format.flush_str_formatter ()) ;
        pp_code rest
  in
  pp_code (List.rev chunk.code) ;
  let code = Opcode.emit (List.rev chunk.code) in
  Bytes.cat header code

let reader (bytes : Bytes.t) =
  let header, size = Header.of_bytes bytes 0 in
  let bytes = Bytes.sub bytes size (Bytes.length bytes - size) in
  let chunk = {empty with header} in
  let func (start : int) =
    let opcode, size = Opcode.of_bytes bytes start in
    Opcode.pp Format.str_formatter opcode ;
    Logger.debug "%s, next: %d" (Format.flush_str_formatter ()) (size + start) ;
    (opcode, size)
  in
  (chunk, func)

let code (chunk : t) = chunk.code

let add_code (chunk : t) (opcode : Opcode.t) : t =
  {chunk with code= Opcode.add_list chunk.code opcode}

let bytecode (chunk : t) = chunk.bytecode
