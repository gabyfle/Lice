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

  val empty : t

  val get_var : t -> identificator -> int64 option

  val get_func : t -> identificator -> int64 option

  val set_var : t -> identificator -> int64 -> t

  val set_func : t -> identificator -> int64 -> t

  val push_scope : t -> t

  val pop_scope : t -> t
end

module Identificator = struct
  type t = identificator

  let get_name (n : t) = match n with `Ident s -> s | `Module (_, s) -> s

  let compare (id : t) (id' : t) =
    let s = get_name id in
    let s' = get_name id' in
    String.compare (fst s) (fst s')
end

module Table = Map.Make (Identificator)

module Scope : SCOPE = struct
  type vars = int64 Table.t

  (* Each int *)
  type funcs = int64 Table.t

  and t = (vars * funcs) list

  let empty : t = []

  let get_var (env : t) (name : identificator) : int64 option =
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
    find_opt name env

  let get_func (env : t) (name : identificator) : int64 option =
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
    find_opt name env

  let set_var (env : t) (name : identificator) (reg : int64) : t =
    let rec aux = function
      | [] ->
          [(Table.add name reg Table.empty, Table.empty)]
      | (scp, md) :: t when Table.mem name scp ->
          let new_scp = Table.add name reg scp in
          (new_scp, md) :: t
      | h :: t ->
          h :: aux t
    in
    aux env

  let set_func (env : t) (name : identificator) (pos : int64) : t =
    let rec aux = function
      | [] ->
          [(Table.empty, Table.add name pos Table.empty)]
      | (scp, funcs) :: t when Table.mem name funcs ->
          let new_funcs = Table.add name pos funcs in
          (scp, new_funcs) :: t
      | h :: t ->
          h :: aux t
    in
    aux env

  let push_scope (env : t) : t =
    match env with [] -> [(Table.empty, Table.empty)] | h :: _ -> h :: env

  let pop_scope (env : t) : t = match env with [] -> env | _ :: t -> t
end
