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
  type t = bool

  type value = bool

  let name = "bool"

  let pretty : Format.formatter -> t -> unit =
   fun fmt x -> Format.fprintf fmt "%b" x

  let compare : t -> t -> int = Bool.compare

  let from : value -> t = Fun.id
end

include Type.Make (S)

let to_string : t -> string = string_of_bool

let neg : t -> t = Stdlib.not

let band : t -> t -> t = ( && )

let bor : t -> t -> t = ( || )

let bxor : t -> t -> t = ( <> )
