{ (* HEADER *) 

open Parser;;
open Lexing;;

exception UnknownChar;;

}

(* REGULAR DEFINITIONS *)

let integer = '-'? ['0'-'9']+
let real    = '-'? ['0'-'9']+['.']['0'-'9']*
let ident   = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* RULES *)

rule lex =
	  parse integer	{ NAT (int_of_string (Lexing.lexeme lexbuf)) }
	| real			{ REAL(float_of_string (Lexing.lexeme lexbuf)) }

	| "true"		{ TRUE }
	| "false"		{ FALSE }

	| "program"		{ PROGRAM }
	| "procedure"	{ PROCEDURE }
	| "var"			{ VAR }
	| "const"		{ CONST }
	| "array"         { ARRAY }
	| "of"            { OF }
	| "int"           { INT }
	| "float"         { FLOAT }
	| "begin"         { BEGIN }
	| "end"           { END }
	| "if"            { IF }
	| "then"          { THEN }
	| "else"          { ELSE }
	| "while"         { WHILE }
	| "do"            { DO }
	| "for"           { FOR }
	| "to"            { TO }
	| "repeat"        { REPEAT }
	| "until"         { UNTIL }
	| "write"         { WRITE }
	| "call"          { CALL }

	| ident           { IDE (Lexing.lexeme lexbuf) }

	| "+"             { PLUS }
	| "-"             { MINUS }
	| "*"             { TIMES }
	| "/"             { DIVISION }
	| "="             { EQUAL }
	| "<="            { LESSEQUAL }
	| "<"             { LESS }
	| "&"             { AND }
	| "|"             { OR }
	| "!"             { NOT }
	| ":="            { ASSIGN }
	| "^"			{ CARET }
	| "@"			{ AT }

	| ";"			{ SEMICOLON }   
	| ":"			{ COLON }
	| ","             { COMMA }
	| ".."            { DOTS }

	| "("             { LP }
	| ")"             { RP }
	| "["             { LBRACKET }
	| "]"             { RBRACKET }

	| [' ' '\t']      { lex lexbuf }
	| ['\n']          { lexbuf.lex_curr_p <- {lexbuf.lex_curr_p
		                with pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1};
		              lex lexbuf }
	| eof             { EOF }

	| _               { raise UnknownChar }

