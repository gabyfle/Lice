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
  type t = (string, statement) Hashtbl.t

  val create : unit -> t

  val get : t -> string -> statement option

  val set : t -> string -> statement -> unit

  val push_scope : t -> t

  val pop_scope : t -> unit

  val dump : t -> unit
end

module Scope = struct
  type t = (string, statement) Hashtbl.t

  let create () : t = Hashtbl.create 10

  let get (env : t) (name : string) : statement option =
    Hashtbl.find_opt env name

  let set (env : t) (name : string) (v : statement) = Hashtbl.replace env name v

  let push_scope (env : t) : t = Hashtbl.copy env

  let pop_scope (env : t) = Hashtbl.clear env

  let inter (env : t) (env' : t) =
    let f key value =
      match (Hashtbl.find_opt env key) with
        | Some v when v = value -> Some v
        | _ -> None
    in
    Hashtbl.filter_map_inplace f env'

  let dump (env : t) =
    let iter k s =
      Printf.printf "Key: %s \nValue: %s \n\n" k (Formatting.stmt_format s)
    in
    Hashtbl.iter iter env
end
