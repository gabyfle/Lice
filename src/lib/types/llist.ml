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

module S = struct
  type t = Base.expr list

  type value = Base.expr

  let name = "list"

  let pretty : Format.formatter -> value list -> unit = fun _ _ -> ()

  let compare : value list -> value list -> int = fun _ _ -> -1

  let from : value -> t = fun v -> [v]
end

include Type.Make (S)

let from_list : value list -> t = fun l -> l

let add : t -> t -> t = ( @ )

let hd : t -> value option = function [] -> None | x :: _ -> Some x

let tl : t -> t option = function [] -> None | _ :: xs -> Some xs
