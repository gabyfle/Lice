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
open Types.Base
open Utils
open Utils.Logger

module type SCOPE = sig
  type t

  val create : unit -> t

  val get : t -> identificator -> statement option

  val set : t -> identificator -> statement -> t

  val push_scope : t -> t

  val pop_scope : t -> t

  val add_module : t -> string -> t

  val dump : t -> unit
end

module Identificator = struct
  type t = string

  let compare = String.compare
end

module Table = Map.Make (Identificator)

let get_name (n : identificator) =
  match n with `Ident s -> s | `Module (_, s) -> s

let get_module (n : identificator) =
  match n with `Ident _ -> None | `Module (m, _) -> Some m

module Scope = struct
  type scp = statement Table.t

  type md = statement Table.t Table.t

  and t = (scp * md) list

  let create () : t = []

  let dump (env : t) =
    let display_modules e =
      let iter k s =
        Logger.debug "[Dump] Module: %s\n" k ;
        Table.iter
          (fun k s ->
            Logger.debug "[Dump] [Module] Key: %s Value: %s \n\n" k
              (Formatting.stmt_format s) )
          s
      in
      Table.iter iter e
    in
    let rec aux = function
      | [] ->
          ()
      | (scp, md) :: t ->
          let iter k s =
            Logger.debug "[Dump] Key: %s \nValue: %s \n\n" k
              (Formatting.stmt_format s)
          in
          Table.iter iter scp ; display_modules md ; aux t
    in
    Logger.debug "Scope DUMP: \n" ;
    aux env

  let get_from_module (env : t) (md_name : string) (name : string) :
      statement option =
    let rec aux = function
      | [] ->
          None
      | (_, md) :: t -> (
        match Table.find_opt md_name md with
        | Some m ->
            Table.find_opt name m
        | None ->
            aux t )
    in
    aux env

  let get (env : t) (name : identificator) : statement option =
    Logger.debug "Getting variable %s\n" (identificator_to_string name) ;
    let md = get_module name in
    let n = get_name name in
    match md with
    | Some m ->
        get_from_module env m (fst n)
    | None ->
        let rec find_opt name = function
          | [] ->
              None
          | (h, _) :: t -> (
            match Table.find_opt name h with
            | Some _ as value ->
                value
            | None ->
                find_opt name t )
        in
        find_opt (fst n) env

  let add_module (env : t) (md_name : string) : t =
    let exists = List.exists (fun (_, md) -> Table.mem md_name md) env in
    if exists then
      raise (Failure (Printf.sprintf "Module %s already exists" md_name))
    else
      try
        let hd = List.hd env in
        let tl = List.tl env in
        let e, md = hd in
        (e, Table.add md_name Table.empty md) :: tl
      with Failure _e ->
        let md = Table.add md_name Table.empty Table.empty in
        (Table.empty, md) :: env

  (* sets a variable inside a given module *)
  let set_module (env : t) (md_name : string) (name : string) (v : statement) :
      t =
    let rec aux = function
      | [] ->
          [ ( Table.empty
            , Table.add md_name (Table.add name v Table.empty) Table.empty ) ]
      | (scp, md) :: t when Table.mem md_name md ->
          let new_md = Table.add name v (Table.find md_name md) in
          let m = Table.add md_name new_md md in
          (scp, m) :: t
      | h :: t ->
          h :: aux t
    in
    aux env

  let set (env : t) (name : identificator) (v : statement) : t =
    Logger.debug "Setting variable %s to value: %s\n"
      (identificator_to_string name)
      (Formatting.stmt_format v) ;
    let md = get_module name in
    let n = get_name name in
    match md with
    | Some m ->
        (* the varible we're trying to set is inside a module *)
        (* let's gets its name and call set_module *)
        set_module env m (fst n) v
    | None ->
        (* the variable we're trying to set is not inside a module *)
        (* let's find the correct scope and set the variable there *)
        let rec aux = function
          | [] ->
              [(Table.add (fst n) v Table.empty, Table.empty)]
          | (scp, md) :: t when Table.mem (fst n) scp ->
              let new_scp = Table.add (fst n) v scp in
              (new_scp, md) :: t
          | h :: t ->
              h :: aux t
        in
        aux env

  let push_scope (env : t) : t =
    match env with [] -> [(Table.empty, Table.empty)] | h :: _ -> h :: env

  let pop_scope (env : t) : t = match env with [] -> env | _ :: t -> t
end
