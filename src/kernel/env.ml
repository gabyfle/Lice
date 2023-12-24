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

open Ast.Tree
open Utils
open Utils.Logger

module type SCOPE = sig
  type t

  val create : unit -> t

  val get : t -> string -> statement option

  val set : t -> string -> statement -> t

  val push_scope : t -> t

  val pop_scope : t -> t

  val dump : t -> unit
end

module Identificator = struct
  type t = string

  let compare = String.compare
end

module Table = Map.Make (Identificator)

module Scope = struct
  type tbl = statement Table.t

  type t = tbl list

  let create () : t = []

  let dump (env : t) =
    let rec aux = function
      | [] ->
          ()
      | h :: t ->
          let iter k s =
            Printf.printf "Key: %s \nValue: %s \n\n" k
              (Formatting.stmt_format s)
          in
          Table.iter iter h ; aux t
    in
    Printf.printf "Scope DUMP: \n" ;
    aux env

  let get (env : t) (name : identificator) : statement option =
    (*Logger.debug "Getting %s\n" name ;*)
    let rec find_opt name = function
      | [] ->
          None
      | h :: _ when Table.mem name h ->
          Table.find_opt name h
      | _ :: t ->
          find_opt name t
    in
    find_opt name env

  let set (env : t) (name : string) (v : statement) : t =
    Logger.debug "Setting %s to %s\n" name (Formatting.stmt_format v) ;
    match env with
    | [] ->
        [Table.add name v Table.empty]
    | h :: t ->
        if Table.mem name h then
          let updated = Table.add name v h in
          updated :: t
        else
          let n = Table.add name v h in
          n :: t

  let push_scope (env : t) : t =
    match env with [] -> [Table.empty] | h :: _ -> h :: env

  let pop_scope (env : t) : t = match env with [] -> env | _ :: t -> t
end
