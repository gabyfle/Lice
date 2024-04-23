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

open Base

(* The value type represent values inside a Lice code. It's basically a wrapper
   around all the functions defined inside Types.Type *)

let to_typ v =
  let from_t = function
    | V_Number _ ->
        T_Number
    | V_String _ ->
        T_String
    | V_List _ ->
        T_List
    | V_Boolean _ ->
        T_Boolean
    | V_Function _ ->
        T_Auto
    | V_Variable _ ->
        T_Auto
    | V_Void ->
        T_Void
  in
  from_t v

let typ_to_string = function
  | T_Number ->
      "number"
  | T_String ->
      "string"
  | T_List ->
      "list"
  | T_Boolean ->
      "boolean"
  | T_Auto ->
      "auto"
  | T_Void ->
      "void"

let get_typ_from_id = function
  | `Ident (_n, t) ->
      t
  | `Module (_id, (_n, t)) ->
      t

let typed_ident_list_to_id : typed_ident list -> identificator list =
  List.map (fun (name, typ) -> `Ident (name, typ))

let name = function
  | V_Number _ ->
      Lnumber.name
  | V_String _ ->
      Lstring.name
  | V_List _ ->
      Llist.name
  | V_Boolean _ ->
      Lbool.name
  | V_Function _ ->
      Lfunction.name
  | V_Variable _ ->
      "variable"
  | V_Void ->
      "void"

let pretty fmt = function
  | V_Number n ->
      Format.fprintf fmt "%a" Lnumber.pretty n
  | V_String s ->
      Format.fprintf fmt "%a" Lstring.pretty s
  | V_List l ->
      Format.fprintf fmt "%a" Llist.pretty l
  | V_Boolean b ->
      Format.fprintf fmt "%a" Lbool.pretty b
  | V_Function f ->
      Format.fprintf fmt "%a" Lfunction.pretty f
  | V_Variable v -> (
    match v with
    | `Ident (id, _) ->
        Format.fprintf fmt "<variable> %s" id
    | `Module (id, (id', _)) ->
        Format.fprintf fmt "<variable> %s..%s" id id' )
  | V_Void ->
      Format.fprintf fmt "Void"

let compare v v' =
  match (v, v') with
  | V_Number n, V_Number n' ->
      Lnumber.compare n n'
  | V_String s, V_String s' ->
      Lstring.compare s s'
  | V_List l, V_List l' ->
      Llist.compare l l'
  | V_Boolean b, V_Boolean b' ->
      Lbool.compare b b'
  | _ ->
      failwith "Cannot compare values of different types"

let eq v v' =
  match (v, v') with
  | V_Number n, V_Number n' ->
      Lnumber.eq n n'
  | V_String s, V_String s' ->
      Lstring.eq s s'
  | V_List l, V_List l' ->
      Llist.eq l l'
  | V_Boolean b, V_Boolean b' ->
      Lbool.eq b b'
  | _ ->
      false

let neq v v' = match eq v v' with b -> not b

let to_string = function
  | V_Number n ->
      Lnumber.to_string n
  | V_String s ->
      Lstring.to_string s
  | V_List l ->
      Llist.to_string l
  | V_Boolean b ->
      Lbool.to_string b
  | V_Function f ->
      Lfunction.to_string f
  | V_Variable v -> (
    match v with
    | `Ident (id, _) ->
        id
    | `Module (id, (id', _)) ->
        id ^ ".." ^ id' )
  | V_Void ->
      ""

let add v v' =
  match (v, v') with
  | V_Number n, V_Number n' ->
      V_Number (Lnumber.add n n')
  | V_String s, V_String s' ->
      V_String (Lstring.add s s')
  | V_List l, V_List l' ->
      V_List (Llist.add l l')
  | V_Boolean b, V_Boolean b' ->
      V_Boolean (Lbool.add b b')
  | V_Function f, V_Function f' ->
      V_Function (Lfunction.add f f')
  | V_Void, V_Void ->
      V_Void
  | _ ->
      failwith "Cannot add values of different types"

let sub v v' =
  match (v, v') with
  | V_Number n, V_Number n' ->
      V_Number (Lnumber.sub n n')
  | V_String s, V_String s' ->
      V_String (Lstring.sub s s')
  | V_List l, V_List l' ->
      V_List (Llist.sub l l')
  | V_Boolean b, V_Boolean b' ->
      V_Boolean (Lbool.sub b b')
  | V_Function f, V_Function f' ->
      V_Function (Lfunction.sub f f')
  | V_Void, V_Void ->
      V_Void
  | _ ->
      failwith "Cannot sub values of different types"

let mul v v' =
  match (v, v') with
  | V_Number n, V_Number n' ->
      V_Number (Lnumber.mul n n')
  | V_String s, V_String s' ->
      V_String (Lstring.mul s s')
  | V_List l, V_List l' ->
      V_List (Llist.mul l l')
  | V_Boolean b, V_Boolean b' ->
      V_Boolean (Lbool.mul b b')
  | V_Function f, V_Function f' ->
      V_Function (Lfunction.mul f f')
  | V_Void, V_Void ->
      V_Void
  | _ ->
      failwith "Cannot mul values of different types"

let div v v' =
  match (v, v') with
  | V_Number n, V_Number n' ->
      V_Number (Lnumber.div n n')
  | V_String s, V_String s' ->
      V_String (Lstring.div s s')
  | V_List l, V_List l' ->
      V_List (Llist.div l l')
  | V_Boolean b, V_Boolean b' ->
      V_Boolean (Lbool.div b b')
  | V_Function f, V_Function f' ->
      V_Function (Lfunction.div f f')
  | V_Void, V_Void ->
      V_Void
  | _ ->
      failwith "Cannot div values of different types"

let neg = function
  | V_Number n ->
      V_Number (Lnumber.neg n)
  | V_String s ->
      V_String (Lstring.neg s)
  | V_List l ->
      V_List (Llist.neg l)
  | V_Boolean b ->
      V_Boolean (Lbool.neg b)
  | V_Function f ->
      V_Function (Lfunction.neg f)
  | V_Variable _ | V_Void ->
      V_Void

let md v v' =
  match (v, v') with
  | V_Number n, V_Number n' ->
      V_Number (Lnumber.md n n')
  | V_String s, V_String s' ->
      V_String (Lstring.md s s')
  | V_List l, V_List l' ->
      V_List (Llist.md l l')
  | V_Boolean b, V_Boolean b' ->
      V_Boolean (Lbool.md b b')
  | V_Function f, V_Function f' ->
      V_Function (Lfunction.md f f')
  | V_Void, V_Void ->
      V_Void
  | _ ->
      failwith "Cannot md values of different types"

let band v v' =
  match (v, v') with
  | V_Number n, V_Number n' ->
      V_Number (Lnumber.band n n')
  | V_String s, V_String s' ->
      V_String (Lstring.band s s')
  | V_List l, V_List l' ->
      V_List (Llist.band l l')
  | V_Boolean b, V_Boolean b' ->
      V_Boolean (Lbool.band b b')
  | V_Function f, V_Function f' ->
      V_Function (Lfunction.band f f')
  | V_Void, V_Void ->
      V_Void
  | _ ->
      failwith "Cannot band values of different types"

let bor v v' =
  match (v, v') with
  | V_Number n, V_Number n' ->
      V_Number (Lnumber.bor n n')
  | V_String s, V_String s' ->
      V_String (Lstring.bor s s')
  | V_List l, V_List l' ->
      V_List (Llist.bor l l')
  | V_Boolean b, V_Boolean b' ->
      V_Boolean (Lbool.bor b b')
  | V_Function f, V_Function f' ->
      V_Function (Lfunction.bor f f')
  | V_Void, V_Void ->
      V_Void
  | _ ->
      failwith "Cannot bor values of different types"

let bxor v v' =
  match (v, v') with
  | V_Number n, V_Number n' ->
      V_Number (Lnumber.bxor n n')
  | V_String s, V_String s' ->
      V_String (Lstring.bxor s s')
  | V_List l, V_List l' ->
      V_List (Llist.bxor l l')
  | V_Boolean b, V_Boolean b' ->
      V_Boolean (Lbool.bxor b b')
  | V_Function f, V_Function f' ->
      V_Function (Lfunction.bxor f f')
  | V_Void, V_Void ->
      V_Void
  | _ ->
      failwith "Cannot bxor values of different types"
