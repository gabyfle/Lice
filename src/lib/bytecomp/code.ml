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

type t = Chunk.t list

let empty = []

let is_empty = function [] -> true | _ -> false

let add (chunk : Chunk.t) (t : t) = List.rev (chunk :: t)

let get (t : t) = match t with [] -> None | h :: _ -> Some h

let set (chunk : Chunk.t) (code : t) : t =
  match code with [] -> [chunk] | _ :: t -> chunk :: t

let emit (t : t) =
  let rec aux acc = function
    | [] ->
        acc
    | chunk :: t ->
        aux (Bytes.cat acc (Chunk.emit chunk)) t
  in
  aux Bytes.empty t
