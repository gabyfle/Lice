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

module type LState = sig
  type t

  val empty : t
  (**
      [empty] creates an empty state of the Lice language *)

  val version : t -> string
  (**
      [version lstate] returns the actual version used for the Lice interpreter and library *)

  val do_string : t -> string -> t
  (**
      [do_string lstate code] reads the string [code] as code, compiles it and execute it inside the [lstate] context *)

  val do_file : t -> string -> t
  (**
      [dofile lstate file] reads the [file] string as a file path and then execute the content inside the [lstate] context as Lice code *)
end

module LState : LState

val bytecode_viewer : string -> string
(**
    [bytecode_viewer code] returns the bytecode of the [code] string in a pretty format *)
