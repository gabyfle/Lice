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

module type Env = sig
  type t

  val empty : t

  val get_var : t -> int -> Base.t option

  val set_var : t -> int -> Base.t -> t

  val push_scope : t -> t

  val pop_scope : t -> t

  val dump : t -> unit
end

module Integer = struct
  type t = int

  let compare = Int.compare
end

module Scope = Map.Make (Integer)

module Environment : Env = struct
  type t = Base.t Scope.t list

  let empty = [Scope.empty]

  let get_var (scope : t) id =
    let rec aux = function
      | [] ->
          None
      | h :: t -> (
        match Scope.find_opt id h with Some v -> Some v | None -> aux t )
    in
    aux scope

  let set_var (scope : t) (id : int) (value : Base.t) =
    match scope with
    | [] ->
        failwith "Empty scope"
    | h :: t ->
        Scope.update id (fun _ -> Some value) h :: t

  let push_scope (scope : t) = Scope.empty :: scope

  let pop_scope (scope : t) =
    match scope with [] -> failwith "Empty scope" | _ :: t -> t

  let dump (scope : t) =
    List.iter
      (fun s ->
        Scope.iter
          (fun k v -> Printf.printf "%d -> %s\n" k (Value.to_string v))
          s )
      scope
end
