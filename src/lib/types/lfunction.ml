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
  type t = string

  type value = int
  (* for the moment, a function value can be created from its "address" in
     code *)

  (* as we'll write the virtual machine, this is subject to change *)

  let name = "function"

  let pretty : Format.formatter -> t -> unit =
   fun fmt x -> Format.fprintf fmt "function<%s>" x

  let compare : t -> t -> int = failwith "Functional values"

  let from : value -> t = fun x -> string_of_int x
end

include Type.Make (S)
