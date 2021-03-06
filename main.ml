open Syntaxtree;;
open Lexer;;
open Parser;;
open Interpreter;;
open Lexing;;

let lexbuf = Lexing.from_channel stdin
in
    let 
        absyntaxtree = (
        try
            program lex lexbuf 
        with _ -> NoProg
        )
    in
        (
         match absyntaxtree with
              Program(d,p,c) ->
                 print_string ">>> The program is syntactically correct!\n";
                 run absyntaxtree
            | NoProg -> print_string ("Errore sintattico alla riga " ^ (string_of_int lexbuf.lex_start_p.pos_lnum) ^ "\n"); exit 1
        );;
