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

open Types.Base

module type SCOPE = sig
  type t

  val create : unit -> t

  val get_var : t -> identificator -> int64 option

  val get_func : t -> identificator -> int64 option

  val set_var : t -> identificator -> int64 -> t

  val set_func : t -> identificator -> int64 -> t

  val push_scope : t -> t

  val pop_scope : t -> t
end

module Identificator = struct
  type t = string

  let compare = String.compare
end

module Table = Map.Make (Identificator)

let get_name (n : identificator) =
  match n with `Ident s -> s | `Module (_, s) -> s

module Scope = struct
  type vars = int64 Table.t

  (* Each int *)
  type funcs = int64 Table.t

  and t = (vars * funcs) list

  let create () : t = []

  let get_var (env : t) (name : identificator) : int64 option =
    let n = get_name name in
    let rec find_opt name = function
      | [] ->
          None
      | (vars, _) :: t -> (
        match Table.find_opt name vars with
        | Some _ as value ->
            value
        | None ->
            find_opt name t )
    in
    find_opt (fst n) env

  let get_func (env : t) (name : identificator) : int64 option =
    let n = get_name name in
    let rec find_opt name = function
      | [] ->
          None
      | (_, funcs) :: t -> (
        match Table.find_opt name funcs with
        | Some _ as value ->
            value
        | None ->
            find_opt name t )
    in
    find_opt (fst n) env

  let set_var (env : t) (name : identificator) (reg : int64) : t =
    let n = get_name name in
    let rec aux = function
      | [] ->
          [(Table.add (fst n) reg Table.empty, Table.empty)]
      | (scp, md) :: t when Table.mem (fst n) scp ->
          let new_scp = Table.add (fst n) reg scp in
          (new_scp, md) :: t
      | h :: t ->
          h :: aux t
    in
    aux env

  let set_func (env : t) (name : identificator) (pos : int64) : t =
    let n = get_name name in
    let rec aux = function
      | [] ->
          [(Table.empty, Table.add (fst n) pos Table.empty)]
      | (scp, funcs) :: t when Table.mem (fst n) funcs ->
          let new_funcs = Table.add (fst n) pos funcs in
          (scp, new_funcs) :: t
      | h :: t ->
          h :: aux t
    in
    aux env

  let push_scope (env : t) : t =
    match env with [] -> [(Table.empty, Table.empty)] | h :: _ -> h :: env

  let pop_scope (env : t) : t = match env with [] -> env | _ :: t -> t
end
