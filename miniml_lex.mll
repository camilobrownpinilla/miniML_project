(* 
                         CS 51 Final Project
                      MiniML -- Lexical Analyzer

 *)

{
  open Printf ;;
  open Miniml_parse ;; (* need access to parser's token definitions *)

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keyword_table = 
    create_hashtable 8 [
                       ("if", IF);
                       ("in", IN);
                       ("then", THEN);
                       ("else", ELSE);
                       ("not", NOT);
                       ("sin", SIN);
                       ("cos", COS);
                       ("let", LET);
                       ("raise", RAISE);
                       ("rec", REC);
                       ("true", TRUE);
                       ("false", FALSE);
                       ("not", NOT);
                       ("fun", FUNCTION);
                       ("function", FUNCTION);
                     ]
                     
  let sym_table = 
    create_hashtable 8 [
                       ("=", EQUALS);
                       ("<>", NOTEQUALS);
                       ("<", LESSTHAN);
                       ("<=", LESSTHANEQ);
                       (">", GREATERTHAN);
                       (">=", GREATERTHANEQ);
                       (".", DOT);
                       ("->", DOT);
                       (";;", EOF);
                       ("~-", NEG);
                       ("+", PLUS);
                       ("-", MINUS);
                       ("*", TIMES);
                       ("/", DIV);
                       ("**", EXP);
                       ("^", CONCAT);
                       ("&&", AND);
                       ("||", OR);
                       ("(", OPEN);
                       (")", CLOSE)
                     ]
}

let digit = ['0'-'9']
let float = ['0'-'9']+ ['.'] ['0'-'9']*
let string = ['"'] ['a'-'z'] ['a'-'z'] ['a'-'z']* ['"']
let char = ['''] ['a'-'z'] [''']
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let sym = ['(' ')'] | (['$' '&' '*' '+' '-' '/' '=' '<' '>' '^'
                        '|' '.' '~' ';' '!' '?' '%' ':' '#']+)

rule token = parse
  | digit+ as inum
        { let num = int_of_string inum in
          INT num
        }
  | float as fnum
        { let num = float_of_string fnum in
          FLOAT num
        }
  | string as s'
        {
          let s = String.sub s' 1 (String.length s' - 2) in
          STRING s
        }
  | char as c'
        {
          let c = String.get c' 1 in
          CHAR c
        }
  | id as word
        { try
            let token = Hashtbl.find keyword_table word in
            token 
          with Not_found ->
            ID word
        }
  | sym as symbol
        { try
            let token = Hashtbl.find sym_table symbol in
            token
          with Not_found ->
            printf "Ignoring unrecognized token: %s\n" symbol;
            token lexbuf
        }
  | '{' [^ '\n']* '}'   { token lexbuf }    (* skip one-line comments *)
  | [' ' '\t' '\n']     { token lexbuf }    (* skip whitespace *)
  | _ as c                                  (* warn & skip unrecognized chars *)
        { printf "Ignoring unrecognized character: %c\n" c;
          token lexbuf
        }
  | eof
        { raise End_of_file }
