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

module type S = sig
  type t

  type value

  val name : string

  val pretty : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val from : value -> t
end

(* Type signature for Lice type *)
module type T = sig
  type t

  type value

  val name : string

  val pretty : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val from : value -> t

  val eq : t -> t -> bool

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val neg : t -> t

  val md : t -> t -> t

  val band : t -> t -> t

  val bor : t -> t -> t

  val bxor : t -> t -> t

  val to_string : t -> string
end

(* Functor to create a Lice type *)
module Make (Ty : S) : T with type t = Ty.t
