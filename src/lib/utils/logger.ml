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

module Colors = struct
  open ANSITerminal

  let str_to_color = function
    | "red" ->
        Red
    | "green" ->
        Green
    | "yellow" ->
        Yellow
    | "blue" ->
        Blue
    | _ ->
        Default

  let print_colored color message =
    let color = str_to_color color in
    print_string [Foreground color] message
end

module LogLevels = struct
  type log_level = Debug | Info | Warning | Error

  let to_string = function
    | Debug ->
        "Debug"
    | Info ->
        "Info"
    | Warning ->
        "Warning"
    | Error ->
        "Error"

  let to_type = function
    | "Debug" ->
        Debug
    | "Info" ->
        Info
    | "Warning" ->
        Warning
    | "Error" ->
        Error
    | _ ->
        Info
end

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

module Logger : LOGGER = struct
  include LogLevels

  let levels = ref []

  let get_level () = !levels

  let set_level (lvls : string list) =
    let log_levels = List.map to_type lvls in
    levels := log_levels

  let add_level (lvl : string) =
    let log_level = to_type lvl in
    if not (List.mem log_level !levels) then levels := log_level :: !levels

  let has_level (lvl : log_level) = List.mem lvl !levels

  let log_message (color : string) (level : log_level)
      (format : ('a, Format.formatter, unit, unit) format4) : 'a =
    Format.kasprintf
      (fun msg ->
        let lvl_name = to_string level in
        let str = Format.sprintf "[%s] %s \n" lvl_name msg in
        if has_level level then Colors.print_colored color str )
      format

  let debug fmt = log_message "blue" Debug fmt

  let warning fmt = log_message "yellow" Warning fmt

  let error fmt = log_message "red" Error fmt

  let info fmt = log_message "info" Info fmt
end
