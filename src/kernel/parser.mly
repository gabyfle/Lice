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
%}

%token <string> IDENT
%token <int> INT
%token <float> FLOAT
%token <bool> BOOLEAN

%token <string> STRING_VALUE

(* Types annotation tokens *)
%token NUMBER
%token STRING
%token MAP
%token LIST
%token BOOL
%token VOID

(* Type specific tokens *)
%token LBRACKET
%token RBRACKET

(* Pattern matching *)
%token MATCH
%token WITH
%token PIPE
%token ARROW
%token WILDCARD

%token COMMA
%token SEMICOLON
%token COLON
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

%inline cases:
  l = separated_list(PIPE, pattern_match)
    { l }

let lprog :=
  | EOF; { [] }
  | s = statement; EOF; { [ s ] }
  | s = statement; EOL*;  sl = lprog; { s :: sl }

let binop ==
  | PLUS; { Plus }
  | MINUS; { Minus }
  | ASTERISK; { Multiply }
  | SLASH; { Divide }
  | MOD; { Mod }

let terminal ==
  | i = INT; { Number (float_of_int i) }
  | i = FLOAT; { Number i }
  | i = IDENT; { Variable (i, T_Auto) }
  | b = BOOLEAN; { Boolean (b) }
  | s = STRING_VALUE; { String(s) }
  | LBRACKET; elems=separated_list(SEMICOLON, expr); RBRACKET; { List(elems) }

let block ==
  | LBRACE; stmts = list(statement); RBRACE;
  { Block($startpos, stmts) }

let parenthesis ==
  | LPAREN; p = expr; RPAREN;
    { p }

let typ ==
  | { T_Auto }
  | NUMBER; { T_Number }
  | STRING; { T_String }
  | LIST; { T_List }
  | BOOL; { T_Boolean }
  | VOID; { T_Void }

let assign ==
  | LET; t = typ; p = IDENT; ASSIGN; e = expr;
  { Assign ($startpos, (p, t), e)}
  | p = IDENT; ASSIGN; e = expr;
  { Assign ($startpos, (p, T_Auto), e) }

let pattern ==
  | b = BOOLEAN; { Boolean (b) }
  | n = INT; { Number (float_of_int n) }
  | n = FLOAT; { Number (n) }
  | s = STRING_VALUE; { String(s) }
  | WILDCARD; { Empty }

let pattern_match ==
  | r = pattern; ARROW; b = block;
    { (r, b) }

let matching ==
  | MATCH; LPAREN; expression = expr; RPAREN; WITH; c = cases;
  { Match($startpos, expression, c) }

let binary_operator ==
  | a = expr; op = binop; b = expr;
    { BinOp(op, a, b) }

(* Parse the parameters of a function with the syntax for annotated types:
  function name(arg1: type ...) *)
let func_def_param ==
  | a = IDENT; COLON; t = typ;
    { (a, t) }

let func_def ==
  | FUNCTION; p = IDENT; LPAREN; args=separated_list(COMMA, func_def_param); RPAREN; COLON; t = typ; b = block;
    { FuncDef($startpos, (p, t), args, b) }

let func_call_param ==
  | p = IDENT; { (p, T_Auto) }

let func_call ==
  | p = IDENT; LPAREN; args=separated_list(COMMA, func_call_param); RPAREN;
    { FuncCall(p, args) }

let return_call ==
  | RETURN;
    { Return(Empty) }
  | RETURN; r = expr;
    { Return(r) }

let statement ==
  | p = expr; SEMICOLON; EOL*; { Expression ($startpos, p, T_Auto) }
  | b = block; { b }
  | f = func_def; { f }
  | r = return_call; SEMICOLON; { r }
  | a = assign; SEMICOLON; { a }
  | m = matching; { m }

let expr :=
  | p = parenthesis; { p }
  | b = binary_operator; { b }
  | f = func_call; { f }
  | t = terminal; { t }
