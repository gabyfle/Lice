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

type typ = T_Number | T_String | T_List | T_Boolean | T_Auto | T_Void

type t =
  | V_Number of Lnumber.t
  | V_String of Lstring.t
  | V_Boolean of Lbool.t

type value = Empty | Val of t | Variable of typed_ident

val to_typ : t -> typ

val typ_to_string : typ -> string

val name : t -> string

val pretty : t -> Format.formatter -> unit

val compare : t -> t -> int

val eq : t -> t -> bool

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
