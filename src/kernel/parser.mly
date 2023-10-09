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
%token FUNCTION
%token RETURN
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

%right ASSIGN
%left PLUS MINUS
%left ASTERISK SLASH MOD

%%

%inline binop:
| PLUS { Plus }
| MINUS { Minus }
| ASTERISK { Multiply }
| SLASH { Divide }
| MOD { Mod }

let lprog :=
  | EOF; { [] }
  | s = statement; EOF; { [ s ] }
  | s = statement; EOL*;  sl = lprog; { s :: sl }

let terminal ==
  | i = INT; { Number (float_of_int i) }
  | i = FLOAT; { Number i }
  | i = IDENT; { Variable i }

let block ==
  | LBRACE; stmts = list(statement); RBRACE;
  { Block($startpos, stmts) }

let assign ==
    | LET; p = IDENT; ASSIGN; e = expr;
    { Assign (p, e)}
    | p = IDENT; ASSIGN; e = expr;
    { Assign(p, e) }

let parenthesis ==
  | LPAREN; p = expr; RPAREN;
    { p }

let binary_operator ==
  | a = expr; op = binop; b = expr;
    { BinOp(op, a, b) }

let func_def ==
  | FUNCTION; p = IDENT; LPAREN; args=separated_list(COMMA, IDENT); RPAREN; b = block;
    { FuncDef($startpos, p, args, b) }

let func_call ==
  | p = IDENT; LPAREN; args=separated_list(COMMA, IDENT); RPAREN;
    { FuncCall(p, args) }

let return_call ==
  | RETURN; r = expr;
    { Return(r) }

let statement ==
  | p = expr; SEMICOLON; { Expression ($startpos, p) }
  | b = block; { b }
  | f = func_def; { f }

let expr :=
  | a = assign; { a }
  | p = parenthesis; { p }
  | b = binary_operator; { b }
  | f = func_call; { f }
  | r = return_call; { r }
  | t = terminal; { t }
