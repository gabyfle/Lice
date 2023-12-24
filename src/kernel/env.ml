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
        Printf.printf "Module: %s\n" k ;
        Table.iter
          (fun k s ->
            Printf.printf "Key: %s \nValue: %s \n\n" k
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
            Printf.printf "Key: %s \nValue: %s \n\n" k
              (Formatting.stmt_format s)
          in
          Table.iter iter scp ; display_modules md ; aux t
    in
    Printf.printf "Scope DUMP: \n" ;
    aux env

  let get (env : t) (name : identificator) : statement option =
    let md = get_module name in
    let n = get_name name in
    match md with
    | Some m ->
        let rec find_opt name = function
          | [] ->
              None
          | (_, h) :: _ when Table.mem m h -> (
              let md = Table.find_opt m h in
              match md with Some m -> Table.find_opt name m | None -> None )
          | _ :: t ->
              find_opt name t
        in
        find_opt (fst n) env
    | None ->
        let rec find_opt name = function
          | [] ->
              None
          | (h, _) :: _ when Table.mem name h ->
              Table.find_opt name h
          | _ :: t ->
              find_opt name t
        in
        find_opt (fst n) env

  let add_module (env : t) (md_name : string) : t =
    let exists = List.exists (fun (_, md) -> Table.mem md_name md) env in
    if exists then
      raise (Failure (Printf.sprintf "Module %s already exists" md_name))
    else
      let md = Table.add md_name Table.empty Table.empty in
      (Table.empty, md) :: env

  (* sets a variable inside a given module *)
  let set_module (env : t) (md_name : string) (name : string) (v : statement) :
      t =
    let rec aux = function
      | [] ->
          []
      | (scp, md) :: t when Table.mem md_name md ->
          (* we found the correct module so we can set the correct value for the
             variable *)
          let new_md = Table.add name v (Table.find md_name md) in
          let md = Table.add md_name new_md md in
          (scp, md) :: t
      | h :: t ->
          h :: aux t
    in
    aux env

  let set (env : t) (name : identificator) (v : statement) : t =
    let md = get_module name in
    let n = get_name name in
    Logger.debug "Setting %s to %s\n" (fst n) (Formatting.stmt_format v) ;
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
              []
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
