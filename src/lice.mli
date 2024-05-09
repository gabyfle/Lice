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

(** It provides the main functions to interact with the Lice interpreter.
    To interact with the language, can use the {!LState} module to create
    a new state, set the logs level, execute code and more.  *)

val version : string
(**
    [version] is the current version of the Lice libraries and interpreter *)

module LState : sig
  (**
        The type of the Lice state. Contains everything needed for the Lice VM to run,
        as well as some other metadata. *)
  type t

  val empty : t
  (**
      [empty] creates an empty state. *)

  val set_logs : t -> int -> t
  (**
      [set_logs lstate l] sets the logs level of the Lice interpreter to [l] where [l] is between [0] and [2].
      
       {t
            |          Value          |                               Level                                |
            | :---------------------: | :----------------------------------------------------------------: |
            | [0] {e (default value)} | Prints out [Warnings] and [Errors] only                            |
            | [1] {e (debug)}         | Prints out [Warnings], [Errors] and [Debug] log messages           |
            | [2] {e (verbose)}       | Prints out [Warnings], [Errors], [Debug] and [Info] log messages   |
        }

    *)

  val do_string : t -> string -> t
  (**
      [do_string lstate code] reads the string [code] as code, compiles it and execute it inside the [lstate] context.
    
      
      
    *)

  val do_file : t -> string -> t
  (**
      [dofile lstate file] reads the [file] string as a file path and then execute the content inside the [lstate] context as Lice code *)
end

val bytecode_viewer : string -> string
(**
    [bytecode_viewer code] returns the bytecode of the [code] string in a pretty format *)
