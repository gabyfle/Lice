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
              ["let", LET;
              "function", FUNCTION;
              "return", RETURN;
              "number", NUMBER;
              "string", STRING;
              "map", MAP;
              "list", LIST;
              "bool", BOOL;
              "void", VOID;
              "match", MATCH;
              "with", WITH;
              "if", IF;
              "else", ELSE;
              "true", BOOLEAN(true);
              "false", BOOLEAN(false)]

  let buf = Buffer.create 256

  exception Lexer_Error of string
}

rule token = parse
  | [' ' '\t']+     { token lexbuf; } (* Skip whitespace *)
  | ['\n' ]         { Lexing.new_line lexbuf; token lexbuf }
  | "--[["          { block_comments lexbuf } (* Block comment starting *)
  | '\"'            { string lexbuf } (* String starting *)
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
                    { try
                        Hashtbl.find keyword_table id
                      with Not_found ->
                        IDENT id }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { ASTERISK }
  | '/'             { SLASH }
  | '%'             { MOD }
  | '='             { ASSIGN }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { COMMA }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | ';'             { SEMICOLON }
  | ':'             { COLON }
  | '|'             { PIPE }
  | '_'             { WILDCARD }
  | "->"            { ARROW }
  | '['             { LBRACKET }
  | ']'             { RBRACKET }
  | eof             { EOF }
  | _               { ILLEGAL }

and block_comments = parse (* we're skipping everything inside multilines comments *)
  | "]]" { token lexbuf }
  | "\n" { Lexing.new_line lexbuf; block_comments lexbuf }
  | _    { block_comments lexbuf }

and string = parse
  | '\"' { STRING_VALUE (Buffer.contents buf) }
  | _ as char {
      Buffer.add_char buf char;
      string lexbuf
    }
  | eof { raise (Lexer_Error "Non-terminated string") }
