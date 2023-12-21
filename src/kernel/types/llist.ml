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
  type t = Base.value list

  type value = Base.value

  let name = "list"

  let pretty : Format.formatter -> value list -> unit =
   fun ppf l ->
    let rec aux ppf = function
      | [] ->
          ()
      | x :: [] ->
          Format.fprintf ppf "%a" pretty_val x
      | x :: xs ->
          Format.fprintf ppf "%a, %a" pretty_val x aux xs
    and pretty_val ppf = function
      | Base.Const t ->
          pretty_base ppf t
      | Base.V_Var (id, _) ->
          Format.fprintf ppf "%s" id
    and pretty_base ppf = function
      | Base.V_Number n ->
          Lnumber.pretty ppf n
      | Base.V_String s ->
          Lstring.pretty ppf s
      | Base.V_Boolean b ->
          Lbool.pretty ppf b
      | Base.V_List l ->
          Format.fprintf ppf "[" ; aux ppf l ; Format.fprintf ppf "]"
      | Base.V_Void ->
          Format.fprintf ppf ""
    in
    Format.fprintf ppf "[%a]" aux l

  let compare : value list -> value list -> int =
   fun l l' ->
    let rec aux = function
      | [], [] ->
          0
      | [], _ ->
          -1
      | _, [] ->
          1
      | x :: xs, y :: ys ->
          compare_val x y + aux (xs, ys)
    and compare_val v v' =
      match (v, v') with
      | Base.Const t, Base.Const t' ->
          compare_base t t'
      | Base.V_Var (id, _), Base.V_Var (id', _) ->
          String.compare id id'
      | _ ->
          0
    and compare_base v v' =
      match (v, v') with
      | Base.V_Number n, Base.V_Number n' ->
          Lnumber.compare n n'
      | Base.V_String s, Base.V_String s' ->
          Lstring.compare s s'
      | Base.V_Boolean b, Base.V_Boolean b' ->
          Lbool.compare b b'
      | _ ->
          failwith "Invalid comparison"
    in
    aux (l, l')

  let from : value -> t = fun v -> [v]
end

include Type.Make (S)

let add : t -> t -> t = ( @ )

let hd : t -> value option = function [] -> None | x :: _ -> Some x

let tl : t -> t = function [] -> failwith "tl: empty list" | _ :: xs -> xs
