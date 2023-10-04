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

{
    open Parser
    let keyword_table = Hashtbl.create 20
    let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
                ["let", LET;]
}

rule token = parse
    [' ' '\t']     { token lexbuf }
    | ['\n' ]        { EOL }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
               { try
                   Hashtbl.find keyword_table id
                 with Not_found ->
                   IDENT id }
    | '+'          { PLUS }
    | '-'          { MINUS }
    | '*'          { ASTERISK }
    | '/'          { SLASH }
    | '%'          { MOD }
    | '='          { ASSIGN }
    | '('          { LPAREN }
    | ')'          { RPAREN }
    | '{'          { LBRACE }
    | '}'          { RBRACE }
    | ';'          { SEMICOLON }
    | _            { ILLEGAL }

{
  exception LexError of string
}
