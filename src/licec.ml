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

open Lice

let exec_file levels file =
  let lvm = LState.set_logs LState.empty levels in
  ignore (LState.do_file lvm file)

let () =
  let llevels = ref 0 in
  let lfile = ref "" in
  Arg.parse
    [ ("-l", Arg.Set_int llevels, "Set log levels")
    ; ("-f", Arg.Set_string lfile, "Set log file") ]
    (fun _ -> ())
    "Usage: lice [-l levels] [-f file]" ;
  exec_file !llevels !lfile
