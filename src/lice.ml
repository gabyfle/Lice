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

open Bytecomp
open Utils.Logger

let version = "0.0.1-dev"

module type LState = sig
  type t

  val empty : t
  (**
      [empty] creates an empty state of the Lice language *)

  val set_logs : t -> int -> t
  (**
      [set_logs lstate l] sets the logs level of the Lice interpreter to [l] *)

  val version : t -> string
  (**
      [version lstate] returns the actual version used for the Lice interpreter and library *)

  val do_string : t -> string -> t
  (**
      [do_string lstate code] reads the string [code] as code, compiles it and execute it inside the [lstate] context *)

  val do_file : t -> string -> t
  (**
      [dofile lstate file] reads the [file] string as a file path and then execute the content inside the [lstate] context as Lice code *)
end

let llevels =
  [ ["Warning"; "Error"]
  ; ["Debug"; "Warning"; "Error"]
  ; ["Info"; "Debug"; "Warning"; "Error"] ]

module LState = struct
  type t = {version: string; vm: Lvm.t; _llevel: int}

  let empty = {version; vm= Lvm.empty; _llevel= 0}

  let version t = t.version

  let set_logs (state : t) (level : int) =
    Logger.set_level (List.nth llevels level) ;
    {state with _llevel= level}

  let do_string (state : t) (code : string) =
    let ast = Kernel.parse_code code in
    let code = Compiler.compile ast in
    let vm = Lvm.load state.vm code in
    let vm = Lvm.do_code vm in
    {state with vm}

  let do_file (state : t) (file : string) =
    let ast = Kernel.parse_file file in
    let code = Compiler.compile ast in
    let vm = Lvm.load state.vm code in
    let vm = Lvm.do_code vm in
    {state with vm}
end

let bytecode_viewer (code : string) =
  let ast = Kernel.parse_code code in
  let code = Compiler.compile ast in
  Compiler.dump_code code
