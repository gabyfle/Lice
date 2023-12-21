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

module S = struct
  type t = Base.t list

  type value = Base.value

  let name = "list"

  let rec pretty : Format.formatter -> t -> unit =
   fun ppf l ->
    let rec aux ppf = function
      | [] ->
          ()
      | x :: [] ->
          Format.fprintf ppf "%a" pretty_value x
      | x :: xs ->
          Format.fprintf ppf "%a, %a" pretty_value x aux xs
    and pretty_value ppf = function
      | LNumber n ->
          Lnumber.pretty ppf n
      | LString s ->
          Lstring.pretty ppf s
      | LBool b ->
          Lbool.pretty ppf b
      | LList l ->
          pretty ppf l
    in
    Format.fprintf ppf "[%a]" aux l

  let compare : t -> t -> int =
   fun l1 l2 ->
    let rec aux = function
      | [], [] ->
          0
      | [], _ ->
          -1
      | _, [] ->
          1
      | x :: xs, y :: ys ->
          compare_value x y + aux (xs, ys)
    and compare_value v1 v2 =
      match (v1, v2) with
      | LNumber n1, LNumber n2 ->
          Lnumber.compare n1 n2
      | LString s1, LString s2 ->
          Lstring.compare s1 s2
      | LBool b1, LBool b2 ->
          Lbool.compare b1 b2
      | _ ->
          0
    in
    aux (l1, l2)

  let from : value -> t = fun v -> [v]
end

include Type.Make (S)

let add : t -> t -> t = ( @ )
