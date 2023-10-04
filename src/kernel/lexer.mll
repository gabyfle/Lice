{
  open Parser
  let keyword_table = Hashtbl.create 20

  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              ["let", LET]
}

rule token = parse
  | [' ' '\t']+     { token lexbuf } (* Skip whitespace *)
  | ['\n' ]         { EOL }
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
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | ';'             { SEMICOLON }
  | eof             { EOF }
  | _               { ILLEGAL }
