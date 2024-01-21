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

open Types

type register = Base.t

type registers =
  { a: register
  ; b: register
  ; c: register
  ; d: register
  ; e: register
  ; f: register
  ; g: register
  ; h: register
  ; i: register
  ; pc: int64 }

type 'a cpu = {registers: registers; memory: 'a}

type t = Base.t cpu

let empty =
  { registers=
      { a= Base.V_Void
      ; b= Base.V_Void
      ; c= Base.V_Void
      ; d= Base.V_Void
      ; e= Base.V_Void
      ; f= Base.V_Void
      ; g= Base.V_Void
      ; h= Base.V_Void
      ; i= Base.V_Void
      ; pc= 0L }
  ; memory= Base.V_Void }
