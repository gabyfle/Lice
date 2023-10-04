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

%{
  open Ast.Types  (* Include the AST module *)

  (* exception Syntax_Error of string *)
%}

%token <string> IDENT
%token <int> INT
%token <float> FLOAT
%token COMMA
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LET
%token ASSIGN
%token EQUAL
%token PLUS
%token MINUS
%token ASTERISK
%token SLASH
%token MOD
%token ILLEGAL
%token EOF
%token EOL

%type <statement> statement
%type <expr> expr
%start <program> lprog

%%

let lprog :=
  | EOF; { [] }
  | s = statement; SEMICOLON; EOF; { [ s ] }
  | s = statement; SEMICOLON; EOL*; sl = lprog; { s :: sl }

let terminal ==
  | i = INT; { Number (float_of_int i) }
  | i = FLOAT; { Number i }
  | i = IDENT; { Variable i }

let statement ==
  | LET; p = IDENT; ASSIGN; e = expr;
    { Assign ($startpos, p, e)}
  | p = terminal; { Expression ($startpos, p) }

let expr :=
  | p = terminal; PLUS; q = terminal;
    { BinOp (Plus, p, q) }
  | p = terminal; MINUS; q = terminal;
    { BinOp (Minus, p, q) }
  | p = terminal; ASTERISK; q = terminal;
    { BinOp (Multiply, p, q) }
  | terminal 
