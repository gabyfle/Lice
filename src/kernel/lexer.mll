{
  open Parser
  open Utils.Logger
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
              "void", VOID]

  let buf = Buffer.create 256
}

rule token = parse
  | [' ' '\t']+     { token lexbuf } (* Skip whitespace *)
  | ['\n' ]         { token lexbuf }
  | "--[["          { comments lexbuf } (* Comment starting *)
  | '\"'            { string lexbuf } (* String starting *)
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
  | ':'             { COLON }
  | '['             { LBRACKET }
  | ']'             { RBRACKET }
  | eof             { EOF }
  | _               { ILLEGAL }

and comments = parse (* we're skipping everything inside multilines comments *)
  | "]]" { token lexbuf }
  | _    { comments lexbuf }

and string = parse
  | '\"' { STRING_VALUE (Buffer.contents buf) }
  | _ as char {
      Buffer.add_char buf char;
      string lexbuf
    }
  | eof { failwith "Non-terminated string" }
