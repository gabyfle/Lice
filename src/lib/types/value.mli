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

open Base

val to_typ : t -> typ

val typ_to_string : typ -> string

val get_typ_from_id : identificator -> typ

val typed_ident_list_to_id : typed_ident list -> identificator list

val name : t -> string

val pretty : Format.formatter -> t -> unit

val compare : t -> t -> int

val eq : t -> t -> bool

val neq : t -> t -> bool

val to_string : t -> string

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val neg : t -> t

val md : t -> t -> t

val band : t -> t -> t

val bor : t -> t -> t

val bxor : t -> t -> t
