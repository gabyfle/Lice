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

let typing_error (loc : Lexing.position) expected actual =
  let char = loc.pos_cnum in
  let line = loc.pos_lnum in
  Format.sprintf
    "Wrong types. Expected type %s but got type %s at line %d, character %d."
    expected actual line char

let params_number_error (loc : Lexing.position) expected actual =
  let char = loc.pos_cnum in
  let line = loc.pos_lnum in
  Format.sprintf
    "Wrong parameters number. Expected %d parameter(s) but got type %d instead \
     at line %d, character %d."
    expected actual line char

let misc_error (loc : Lexing.position) str =
  let char = loc.pos_cnum in
  let line = loc.pos_lnum in
  Format.sprintf "Langage error: %s. At line %d, character %d." str line char
