{
  open Parser
  open Utils.Logger
  let keyword_table = Hashtbl.create 20

  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              ["let", LET;
              "function", FUNCTION;
              "return", RETURN]
}

rule token = parse
  | [' ' '\t']+     { token lexbuf } (* Skip whitespace *)
  | ['\n' ]         { EOL }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
                    { try
                        Logger.debug "Identifier: %s" id;
                        let q = Hashtbl.find keyword_table id in
                        Logger.debug "Found identifier %s" id; q
                      with Not_found ->
                        Logger.debug "Not found identifier in keywords: %s" id;
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
  | eof             { EOF }
  | _               { ILLEGAL }
