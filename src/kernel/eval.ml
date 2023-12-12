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

open Ast.Types
open Env
open Located_error
open Semantic.Typing
open Utils

module type EVAL = sig
  val exec : program -> unit
end

module Eval : EVAL = struct
  type expr_or_statement = [`Expression of expr | `Statement of statement]

  let get_value env loc ident =
    match Scope.get env ident with
    | None ->
        raise (Located_error (`Undefined_Variable ident, loc))
    | Some h -> (
      match h with
      | Expression (_, e, _) ->
          `Expression e
      | FuncDef (_, _, _, _) as f ->
          `Statement f
      | _ ->
          let str = Printf.sprintf "Variable %s definition error." ident in
          raise (Located_error (`Language_Error str, loc)) )

  let binop_helper env loc v v' op =
    let values =
      match (v, v') with
      | Number k, Number k' ->
          (k, k')
      | Number k, Variable (id, _) ->
          let is_number = is_variable_type env id T_Number in
          if is_number then
            let value =
              match get_value env loc id with
              | `Expression (Number k') ->
                  k'
              | _ ->
                  raise (Located_error (`Not_Number, loc))
            in
            (k, value)
          else raise (Located_error (`Not_Number, loc))
      | Variable (id, _), Number k ->
          let is_number = is_variable_type env id T_Number in
          if is_number then
            let value =
              match get_value env loc id with
              | `Expression (Number k') ->
                  k'
              | _ ->
                  raise (Located_error (`Not_Number, loc))
            in
            (value, k)
          else raise (Located_error (`Not_Number, loc))
      | Variable (id, _), Variable (id', _) ->
          let is_number = is_variable_type env id T_Number in
          let is_number' = is_variable_type env id' T_Number in
          if is_number && is_number' then
            let v = get_value env loc id in
            let v' = get_value env loc id' in
            match (v, v') with
            | `Expression (Number k), `Expression (Number k') ->
                (k, k')
            | _ ->
                raise (Located_error (`Not_Number, loc))
          else raise (Located_error (`Not_Number, loc))
      | _ ->
          raise
            (Located_error
               ( `Language_Error
                   "An unknown error occured while trying to perform a binary \
                    operation on Numbers."
               , loc ) )
    in
    let aux (k, k') = function
      | Plus ->
          Number (k +. k')
      | Minus ->
          Number (k -. k')
      | Multiply ->
          Number (k *. k')
      | Divide ->
          if k' = 0. then raise Division_by_zero else Number (k /. k')
      | Mod ->
          if not (Float.is_integer k') then raise Division_by_zero
          else if k' = 0. then raise Division_by_zero
          else
            let int_k = Int.of_float k in
            let int_k' = Int.of_float k' in
            Number (Float.of_int (int_k mod int_k'))
    in
    aux values op

  let bincomp_helper _env _loc v v' = function
    | Equal ->
        Boolean (v = v')
    | NotEqual ->
        Boolean (v <> v')
    | GEQ ->
        Boolean (v >= v')
    | LEQ ->
        Boolean (v <= v')
    | Greater ->
        Boolean (v > v')
    | Lesser ->
        Boolean (v < v')

  (* compute_list recreates a list object from a syntax like h :: t. [env] is
     the current environement in which we're doing this [loc] is the location of
     the statement that asked to create that list [head] is the head of the list
     we want to create (the h in h :: t) [tail] is an identificator for the list
     tail we want to create *)
  let _compute_list env loc head (tail : identificator) =
    let v = get_value env loc tail in
    match v with
    | `Expression (List (h, t)) -> (
      match h with None -> List (head, t) | Some _ -> List (head, List (h, t)) )
    | _ ->
        raise (Located_error (`Wrong_Type (T_List, T_Auto), loc))

  (* eval_function evaluates a function call inside the AST to remplace it by
     the actual value returned (or not) by the function. this function processes
     the given arguments first, then populates a new scope made out of the
     current one but with the new local variables defined in the function
     definition *)
  let rec eval_function env loc id expr_list =
    let params, stmts =
      match get_value env loc id with
      | `Statement (FuncDef (_, _, p, s)) ->
          (p, s)
      | _ ->
          raise (Located_error (`Not_A_Callable, loc))
    in
    let rec process_args acc = function
      (* we first need to process every argument in order to pass them to the
         function *)
      | [] ->
          acc
      | h :: t ->
          process_args (eval_expr env loc h :: acc) t
    in
    let processed_args = List.rev (process_args [] expr_list) in
    let f_env = Scope.push_scope env in
    (* f_env is a copy of our current env *)
    (* then we want to populate the new env with these processed arguments so
       that inside the function we got our local variables *)
    let iter2 (id, _) param =
      Scope.set f_env id (Expression (loc, param, T_Auto))
    in
    List.iter2 iter2 params processed_args ;
    eval_expr f_env loc (eval_statement f_env stmts)

  and eval_expr env loc = function
    | Empty ->
        Empty
    | (Number _ | String _ | Boolean _ | List (_, _)) as v ->
        v
    | Variable (id, _) -> (
      match get_value env loc id with
      | `Expression e ->
          e
      | `Statement _ ->
          raise
            (Located_error
               ( `Language_Error
                   "An error occurred while trying to get the variable"
               , loc ) ) )
    | BinOp (op, a, b) -> (
      match op with
      | `Compare bincomp ->
          bincomp_helper env loc a b bincomp
      | `Operator binop ->
          binop_helper env loc a b binop )
    | FuncCall (id, expr_list) ->
        eval_function env loc id expr_list

  and eval_statement env = function
    | Return (_, e) ->
        e
    | Expression (loc, e, _typ) ->
        eval_expr env loc e
    | Block (_, _) ->
        Empty
    | Assign (_, _, _) ->
        Empty
    | FuncDef (_, _, _, _) ->
        Empty
    | Match (_, _, _) ->
        Empty
    | If (_, _, _, _) ->
        Empty

  let exec (p : program) =
    Printf.printf "%s" (Formatting.program_format p) ;
    let env = Scope.create () in
    let rec aux = function
      | [] ->
          ()
      | stmt :: t ->
          ignore (eval_statement env stmt) ;
          aux t
    in
    aux p
end
