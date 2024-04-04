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

module type SCOPE = sig
  type t

  val empty : t

  val get_var : t -> Base.identificator -> int64 option

  val get_func : t -> Base.identificator -> int64 option

  val set_var : t -> Base.identificator -> int64 -> t

  val set_func : t -> Base.identificator -> int64 -> t

  val push_scope : t -> t

  val pop_scope : t -> t
end

module Identificator = struct
  type t = Base.identificator

  let get_name (n : t) = match n with `Ident s -> s | `Module (_, s) -> s

  let compare (id : t) (id' : t) =
    let s = get_name id in
    let s' = get_name id' in
    String.compare (fst s) (fst s')
end

module Table = Map.Make (Identificator)

module Scope : SCOPE = struct
  type t = unit

  let empty = ()

  let get_var = failwith "Not implemented"

  let get_func = failwith "Not implemented"

  let set_var = failwith "Not implemented"

  let set_func = failwith "Not implemented"

  let push_scope = failwith "Not implemented"

  let pop_scope = failwith "Not implemented"
end
