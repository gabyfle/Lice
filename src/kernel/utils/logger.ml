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
    | "red" -> Red
    | "green" -> Green
    | "yellow" -> Yellow
    | "blue" -> Blue
    | _ -> Default

  let print_colored color fmt =
    let color = str_to_color color in
    Printf.ksprintf (fun message ->
        print_string [Foreground color] message
      ) fmt
end

type log_level = Debug | Warning | Error | Info

(*
    This is our logger class that handles logging inside the whole
    project. It can handle basic logging with different levels and
    writing output to files. *)
class logger =
  object(self)
    val mutable levels = []

    method get_level () = levels

    method set_level (lvls: log_level list) = levels <- lvls

    method add_level (lvl: log_level) =
      if (List.mem lvl levels) then
        ()
      else
        levels <- lvl :: levels

    method private has_level (lvl: log_level) = List.mem lvl levels

    method debug (message: string) =
      if (self#has_level Debug) then
        Colors.print_colored "blue" "[Debug] %s \n" message
      else
        ()

    method warning (message: string) =
      if (self#has_level Warning) then
        Colors.print_colored "yellow" "[Warning] %s \n" message
      else
        ()

    method error (message: string) =
      if (self#has_level Error) then
        Colors.print_colored "red" "[Error] %s \n" message
      else
        ()

    method info (message: string) =
      if (self#has_level Info) then
        Colors.print_colored "blue" "[Info] %s \n" message
      else
        ()
  end