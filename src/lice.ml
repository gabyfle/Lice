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

open Utils.Logger
open Bytecomp.Opcode
open Types

let () =
  (*let executable_dir = match Sys.argv with | [|_; exec_path|] -> (* Get the
    directory containing the executable *) let exec_dir = Filename.dirname
    exec_path in (* Construct the full path to the test file *) Filename.concat
    exec_dir "tests/perf/exp.lice" | _ -> failwith "Invalid command line
    arguments" in let in_channel = open_in executable_dir in let rec read_code
    lines = try let line = input_line in_channel in read_code (line :: lines)
    with End_of_file -> List.rev lines in let code_lines = read_code [] in
    close_in in_channel ; let _code = String.concat "\n" code_lines in*)
  Logger.set_level ["Warning"; "Info"; "Error"] ;
  let code =
    [ JMP 69
    ; PUSHENV (* we push the environment *)
    ; POP (* we pop the argument passed to the function *)
    ; EXTEND 2 (* we extend the env by adding n to it *)
    ; LOADK 0 (* we load 0 into the acc *)
    ; PUSH (* we push 0 *)
    ; SEARCH 2 (* we search for n in the env *)
    ; EQ (* we compare n with 0 *)
    ; JMPNZ 37 (* if n != 0, we jump to the next instruction *)
    ; LOADK 1 (* we load 1 into the acc *)
    ; PUSH (* we push 1 *)
    ; POPENV
    ; RETURN (* we return *)
    ; LOADK 1 (* we load 1 into the acc *)
    ; PUSH (* we push 1 *)
    ; SEARCH 2 (* we load n into the acc *)
    ; SUB (* we subtract 1 from n and get it into the acc *)
    ; PUSH (* we push n - 1 *)
    ; LOADK 2 (* we load the function factorial into the acc *)
    ; CALL 1 (* we call the function with n *)
    ; SEARCH 2 (* we search for n in the env *)
    ; MUL (* we multiply n by factorial(n - 1) *)
    ; PUSH (* we add the result into the stack *)
    ; POPENV
    ; RETURN (* factorial(n - 1) *)
    ; LOADK 3 (* we load n into the acc *)
    ; PUSH (* we push n *)
    ; LOADK 2 (* we load the function factorial into the acc *)
    ; CALL 1 (* we call the function with n *)
    ; POP
    ; HALT ]
  in
  let bytes = emit code in
  let lvm = Lvm.create () in
  let lvm = Lvm.add_symbol lvm 0 (V_Number (Lnumber.from 0.)) in
  let lvm = Lvm.add_symbol lvm 1 (V_Number (Lnumber.from 1.)) in
  let lvm =
    Lvm.add_symbol lvm 2 (V_Function (Lfunction.from ("factorial", 5)))
  in
  let lvm = Lvm.add_symbol lvm 3 (V_Number (Lnumber.from 10000.)) in
  let lvm = Lvm.load lvm bytes in
  let start = Unix.gettimeofday () in
  let lvm = Lvm.do_code lvm in
  Value.pretty Format.std_formatter (Cpu.get_acc (Lvm.cpu lvm)) ;
  Format.force_newline () ;
  let stop = Unix.gettimeofday () in
  Printf.printf "Execution time: %f\n" (stop -. start)
