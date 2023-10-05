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

module type LOGGER = sig
  type log_level

  val get_level : unit -> log_level list

  val set_level : string list -> unit

  val add_level : string -> unit

  val log_message :
    string -> log_level -> ('a, Format.formatter, unit, unit) format4 -> 'a

  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

  val warning : ('a, Format.formatter, unit, unit) format4 -> 'a

  val error : ('a, Format.formatter, unit, unit) format4 -> 'a

  val info : ('a, Format.formatter, unit, unit) format4 -> 'a
end

module Logger : LOGGER
