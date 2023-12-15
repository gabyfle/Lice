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

open Ast.Types
open Utils

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

  let get (env : t) (name : identificator) : statement option =
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
    let rec aux name v = function
      | [] ->
          [Table.add name v Table.empty]
      | h :: t when Table.mem name h ->
          let replace = function Some _ -> Some v | None -> None in
          Table.update name replace h :: t
      | h :: t ->
          h :: aux name v t
    in
    aux name v env

  let push_scope (env : t) : t =
    let head = List.hd env in
    let n = Table.empty in
    let f _key a b = match (a, b) with (_ as v), _ -> Some v in
    Table.union f head n :: env

  let pop_scope (env : t) : t =
    match env with
    | [] ->
        raise (Failure "Cannot pop empty scope")
    | _ :: t ->
        t

  let dump (env : t) =
    let rec aux = function
      | [] ->
          ()
      | h :: t ->
          let iter k s =
            Printf.printf "Key: %s \nValue: %s \n\n" k
              (Formatting.stmt_format s)
          in
          Printf.printf "Scope: \n" ; Table.iter iter h ; aux t
    in
    aux env
end
