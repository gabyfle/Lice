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

open Kernel
open Utils.Logger
open Eval

let () =
  let executable_dir =
    match Sys.argv with
    | [|_; exec_path|] ->
        (* Get the directory containing the executable *)
        let exec_dir = Filename.dirname exec_path in
        (* Construct the full path to the test file *)
        Filename.concat exec_dir "tests/parser/functions.lice"
    | _ ->
        failwith "Invalid command line arguments"
  in
  let in_channel = open_in executable_dir in
  let rec read_code lines =
    try
      let line = input_line in_channel in
      read_code (line :: lines)
    with End_of_file -> List.rev lines
  in
  let code_lines = read_code [] in
  close_in in_channel ;
  let code = String.concat "\n" code_lines in
  Logger.set_level ["Debug"; "Info"; "Error"] ;
  let ast = parse_code code in
  try
    let t r = r in
    let _ = Errors.handle_type_exception t Eval.exec ast in
    ()
  with _ ->
    Logger.error "Error while evaluating the Lice code. Exiting..." ;
    exit 1
