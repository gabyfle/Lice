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

module type S = sig
  type t

  type value

  val name : string

  val pretty : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val from : value -> t
end

module type T = sig
  type t

  type value

  val name : string

  val pretty : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val from : value -> t

  val eq : t -> t -> bool

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val neg : t -> t

  val md : t -> t -> t

  val band : t -> t -> t

  val bor : t -> t -> t

  val bxor : t -> t -> t

  val to_string : t -> string
end

module Make (Ty : S) : T with type t = Ty.t and type value = Ty.value = struct
  include Ty

  let eq : t -> t -> bool = fun x y -> compare x y = 0

  let add : t -> t -> t = raise (Stdlib.Failure "Not implemented")

  let sub : t -> t -> t = raise (Stdlib.Failure "Not implemented")

  let mul : t -> t -> t = raise (Stdlib.Failure "Not implemented")

  let div : t -> t -> t = raise (Stdlib.Failure "Not implemented")

  let neg : t -> t = raise (Stdlib.Failure "Not implemented")

  let md : t -> t -> t = raise (Stdlib.Failure "Not implemented")

  let band : t -> t -> t = raise (Stdlib.Failure "Not implemented")

  let bor : t -> t -> t = raise (Stdlib.Failure "Not implemented")

  let bxor : t -> t -> t = raise (Stdlib.Failure "Not implemented")

  let to_string : t -> string = raise (Stdlib.Failure "Not implemented")
end

module Generic = struct
  let name (type a) (module M : T with type t = a) : string = M.name

  let pretty (type a) (module M : T with type t = a) (fmt : Format.formatter)
      (x : a) : unit =
    M.pretty fmt x

  let compare (type a) (module M : T with type t = a) (x : a) (y : a) : int =
    M.compare x y

  let from (type a b) (module M : T with type t = a and type value = b)
      (x : M.value) : a =
    M.from x

  let eq (type a) (module M : T with type t = a) (x : a) (y : a) : bool =
    M.eq x y

  let add (type a) (module M : T with type t = a) (x : a) (y : a) : a =
    M.add x y

  let sub (type a) (module M : T with type t = a) (x : a) (y : a) : a =
    M.sub x y

  let mul (type a) (module M : T with type t = a) (x : a) (y : a) : a =
    M.mul x y

  let div (type a) (module M : T with type t = a) (x : a) (y : a) : a =
    M.div x y

  let neg (type a) (module M : T with type t = a) (x : a) : a = M.neg x

  let md (type a) (module M : T with type t = a) (x : a) (y : a) : a = M.md x y

  let band (type a) (module M : T with type t = a) (x : a) (y : a) : a =
    M.band x y

  let bor (type a) (module M : T with type t = a) (x : a) (y : a) : a =
    M.bor x y

  let bxor (type a) (module M : T with type t = a) (x : a) (y : a) : a =
    M.bxor x y

  let to_string (type a) (module M : T with type t = a) (x : a) : string =
    M.to_string x
end

type _ terminal =
  | Number : Lnumber.t -> Lnumber.t terminal
  | String : Lstring.t -> Lstring.t terminal
  | Bool : Lbool.t -> Lbool.t terminal
  | List : Llist.t -> Llist.t terminal

let get_type : type a. a terminal -> a =
 fun x ->
  match x with
  | Number x ->
      (x :> a)
  | String x ->
      (x :> a)
  | Bool x ->
      (x :> a)
  | List x ->
      (x :> a)

let get_module : type a. a terminal -> (module T with type t = a) = function
  | Number _ ->
      (module Lnumber : T with type t = a)
  | String _ ->
      (module Lstring : T with type t = a)
  | Bool _ ->
      (module Lbool : T with type t = a)
  | List _ ->
      (module Llist : T with type t = a)

let filter :
    type a b. a terminal -> b terminal -> (a * b) * (module T with type t = a) =
 fun x y ->
  let module M = (val get_module x) in
  let module N = (val get_module y) in
  if M.name = N.name then ((get_type x, get_type y), (module M))
  else raise (Stdlib.Failure "Not the same type")

let compare : type a. a terminal -> a terminal -> int =
 fun x y ->
  let (x, y), (module M) = filter x y in
  Generic.compare (module M) x y

let eq : type a. a terminal -> a terminal -> bool =
 fun x y ->
  let (x, y), (module M) = filter x y in
  Generic.eq (module M) x y
