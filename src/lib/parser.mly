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
  open Ast.Tree  (* Include the AST module *)
  open Types
  open Types.Base
%}

%token <string> IDENT
%token <int> INT
%token <float> FLOAT
%token <bool> BOOLEAN

%token <string> STRING_VALUE

(* Types annotation tokens *)
%token NUMBER
%token STRING
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

(* Branching *)
%token IF
%token ELSE

(* Modules *)
%token MODULE
%token OPEN

(* General syntax *)
%token COMMA
%token SEMICOLON
%token COLON
%token DOT
%token DOUBLE_COLON
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE

(* Variable assigns *)
%token LET
%token ASSIGN

(* Functions *)
%token FUNCTION
%token RETURN

(* Arithmetics *)
%token PLUS
%token MINUS
%token ASTERISK
%token SLASH
%token MOD

(* Comparison *)
%token EQEQ
%token NOTEQ
%token LEQ
%token GEQ
%token GREATER
%token LESSER

(* Misc *)
%token EOF
%token EOL

%type <statement> statement
%type <expr> expr
%start <program> lprog

%right ASSIGN
%left PLUS MINUS
%left ASTERISK SLASH MOD

%%

let lprog :=
  | EOF; { [] }
  | f = func_def; EOL*; sl = lprog; { f :: sl }
  | s = statement; EOF; { [ s ] }
  | s = statement; EOL*;  sl = lprog; { s :: sl }

let binop ==
  | PLUS; { `Operator(Plus) }
  | MINUS; { `Operator(Minus) }
  | ASTERISK; { `Operator(Multiply) }
  | SLASH; { `Operator(Divide) }
  | MOD; { `Operator(Mod) }
  | EQEQ; { `Compare(Equal) }
  | NOTEQ; { `Compare(NotEqual) }
  | LEQ; { `Compare(LEQ) }
  | GEQ; { `Compare(GEQ) }
  | GREATER; { `Compare(Greater) }
  | LESSER; { `Compare(Lesser) }

let list_terminals :=
  | i = INT; { Terminal((V_Number (Lnumber.from(float_of_int i)))) }
  | i = FLOAT; { Terminal((V_Number (Lnumber.from i))) }
  | b = BOOLEAN; { Terminal((V_Boolean (Lbool.from b))) }
  | s = STRING_VALUE; { Terminal((V_String(Lstring.from (s)))) }
  | l = lists; { l }

let lists :=
  | LBRACKET; _=separated_list(SEMICOLON, expr); RBRACKET;
  {
    Terminal((V_List(Llist.from_list [])))
  }
  | _ = IDENT; DOUBLE_COLON; _ = IDENT;
  {
    (*BinOp(`Cons, Terminal(V_Variable(`Ident(h, T_Auto))), Terminal(V_Variable(`Ident(t, T_Auto))))*)
    Terminal((V_List(Llist.from_list [])))
  }
  | _ = list_terminals; DOUBLE_COLON; _ = IDENT;
  { 
    (*BinOp(`Cons, Terminal((V_List(Llist.from h))), Terminal(V_Variable(`Ident(t, T_Auto))))*)
    Terminal((V_List(Llist.from_list [])))
  }
  | h = list_terminals; DOUBLE_COLON; LBRACKET; RBRACKET;
  {
    Terminal((V_List(Llist.from h)))
  }

let terminal ==
  | i = INT; { Terminal((V_Number (Lnumber.from(float_of_int i)))) }
  | i = FLOAT; { Terminal((V_Number (Lnumber.from i))) }
  | i = IDENT; { Terminal(V_Variable(`Ident(i, T_Auto))) }
  | i = IDENT; DOT; p = IDENT; { Terminal(V_Variable(`Module(i, (p, T_Auto)))) }
  | b = BOOLEAN; { Terminal(V_Boolean (Lbool.from b)) }
  | s = STRING_VALUE; { Terminal(V_String(Lstring.from (s))) }
  | l = lists; { l }

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
  | LET; p = IDENT; COLON; ASSIGN; e = expr;
  { Assign ($startpos, `Ident(p, T_Auto), e)}
  | LET; t = typ; p = IDENT; ASSIGN; e = expr;
  { Assign ($startpos, `Ident(p, t), e)}
  | p = IDENT; ASSIGN; e = expr;
  { Assign ($startpos, `Ident(p, T_Auto), e) }

let pattern ==
  | t = terminal; { t }
  | WILDCARD; { Terminal((V_Void)) }

let match_expr ==
  | MATCH; e = expr; WITH; LBRACE; m = match_cases; RBRACE;
  { Match($startpos, e, m) }

let match_cases :=
  | PIPE; h = match_case; t = match_cases;
  { h :: t }
  | PIPE; h = match_case;
  { [h] }

let match_case :=
  | p = pattern; ARROW; b = statement;
  { (p, [b]) }

let binary_operator ==
  | a = expr; op = binop; b = expr;
    { BinOp(op, a, b) }

(* Parse the parameters of a function with the syntax for annotated types:
  function name(arg1: type ...) *)
let func_def_param ==
  | m = IDENT; DOT; a = IDENT; COLON; t = typ;
    { `Module(m, (a, t)) }
  | a = IDENT; COLON; t = typ;
    { `Ident (a, t) }
  | a = IDENT;
    { `Ident (a, T_Auto) }

let func_def ==
  | FUNCTION; p = IDENT; LPAREN; args=separated_list(COMMA, func_def_param); RPAREN; COLON; t = typ; b = block;
    { 
      let b = match b with
        | Block(_, stmts) -> stmts
        | _ -> [b]
      in
      FuncDef($startpos, `Ident(p, t), args, b)  
    }
  | FUNCTION; p = IDENT; LPAREN; args=separated_list(COMMA, func_def_param); RPAREN; b = block;
    {
      let b = match b with
        | Block(_, stmts) -> stmts
        | _ -> [b]
      in
      FuncDef($startpos, `Ident(p, T_Auto), args, b)
    }

let func_call_param ==
  | p = expr; { p }

let func_call ==
  | p = IDENT; LPAREN; args=separated_list(COMMA, func_call_param); RPAREN;
    { FuncCall(`Ident(p, T_Auto), args) }
  | md = IDENT; DOT; p = IDENT; LPAREN; args=separated_list(COMMA, func_call_param); RPAREN;
    { FuncCall(`Module(md, (p, T_Auto)), args) }

let return_call ==
  | RETURN;
    { Return($startpos, Terminal((V_Void))) }
  | RETURN; r = expr;
    { Return($startpos, r) }

let if_stmt :=
  | IF; LPAREN; e = expr; RPAREN; b1 = statement; ELSE; b2 = statement;
  { If ($startpos, e, b1, b2) }
  | IF; LPAREN; e = expr; RPAREN; b1 = statement;
  { If ($startpos, e, b1, Expression($startpos, Terminal((V_Void)), T_Void)) }

let module_elems := 
  | e = assign; SEMICOLON; EOL*; { e }
  | f = func_def; EOL*; { f }  

let module_body := 
  | LBRACE; stmts = list(module_elems); RBRACE;
  { stmts }

let module_def := 
  | MODULE; id = IDENT; b = module_body;
  { ModuleDef($startpos, id, b) }

let statement ==
  | p = expr; SEMICOLON; EOL*; { Expression ($startpos, p, T_Auto) }
  | b = block; { b }
  | r = return_call; SEMICOLON; { r }
  | a = assign; SEMICOLON; { a }
  | m = match_expr; { m }
  | i = if_stmt; { i }
  | m = module_def; { m }

let expr :=
  | p = parenthesis; { p }
  | b = binary_operator; { b }
  | f = func_call; { f }
  | t = terminal; { t }
