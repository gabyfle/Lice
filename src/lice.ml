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

let version = "0.0.1~dev"

let llevels =
  [ ["Warning"; "Error"]
  ; ["Debug"; "Warning"; "Error"]
  ; ["Info"; "Debug"; "Warning"; "Error"] ]

module LState = struct
  type t = {vm: Lvm.t; _llevel: int}

  let empty = {vm= Lvm.empty; _llevel= 0}

  let set_logs (state : t) (level : int) =
    if level < 0 || level > 2 then
      raise (Invalid_argument "Logs level must be between 0 and 2") ;
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
