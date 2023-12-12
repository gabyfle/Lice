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

  val set : t -> string -> statement -> unit

  val push_scope : t -> t

  val pop_scope : t -> unit

  val dump : t -> unit
end

module Identificator = struct
  type t = string
  let compare = String.compare
end

module Table = Map.Make (Identificator)

module Scope = struct
  type t = statement Table.t

  let create () : t = Table.empty

  let get (env : t) (name : identificator) : statement option =
    Table.find_opt name env

  let set (env : t) (name : string) (v : statement) : t =
    let replace = function
      | Some _ -> Some v
      | None -> None
    in
    Table.update name replace env

  let push_scope (env : t) : t =
    let n = create () in
    let rec f _key a b = match a, b with
      | _ as v, _ -> Some v
    in
    Table.union f env n

  let pop_scope (env : t) =
    let clear _k a = None in
    Table.filter_map clear env

  let dump (env : t) =
    let iter k s =
      Printf.printf "Key: %s \nValue: %s \n\n" k (Formatting.stmt_format s)
    in
    Table.iter iter env
end
