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
  type name = string

  type t = name * int

  (**
      A function is represented by its address, stored in 32-bits *)
  type value = name * int

  let name = "function"

  let pretty : Format.formatter -> t -> unit =
   fun fmt (n, d) -> Format.fprintf fmt "function<%s> at: %d" n d

  let compare (a : t) (b : t) = Stdlib.compare a b

  let from : value -> t = fun x -> x
end

include Type.Make (S)

let address : t -> int = fun (_, x) -> x
