%{ (* HEADER *)

open Syntaxtree;;

%}

%token <string>	IDE
%token <int>	NAT
%token <float>	REAL

%token			TRUE FALSE

%token			PROGRAM PROCEDURE VAR CONST INT FLOAT BEGIN END 
%token			IF THEN ELSE WHILE DO FOR TO REPEAT UNTIL WRITE
%token			ARRAY OF LBRACKET RBRACKET DOTS CALL

%token			PLUS MINUS TIMES DIVISION EQUAL LESSEQUAL LESS AND OR NOT 
%token			ASSIGN CARET AT

%token			SEMICOLON COLON COMMA

%token			LP RP

%token			EOF

%start program
%type<Syntaxtree.program> program

%%

program 
    : PROGRAM opt_dec_list opt_proc_list cmd EOF  { Program($2,$3,$4) }
    ;

opt_dec_list
    :                                             { [] }
    | dec_list                                    { $1 }
    ;

dec_list
    : dec                                         { [$1] }
    | dec SEMICOLON dec_list                      { $1::$3 }
    ;

dec
    : VAR ide COLON gType                         { Dec($2,$4) }
    | CONST ide COLON bType ASSIGN aexp           { Dec($2, Const($4, $6)) }
    ;

gType
    : bType                                       { Basic($1) }
    | ARRAY LBRACKET NAT DOTS NAT RBRACKET OF bType { Vector($8,$3,$5) }
    | pType										  { Pointer($1) }
    ;

pType
	: bType										  { SPointer($1) }
	| CARET pType								  { MPointer($2) }

bType
    : INT                                         { Int }
    | FLOAT                                       { Float }
    ;


opt_proc_list
    :                                             { [] }
    | proc_list                                   { $1 }
    ;

proc_list
    : proc                                        { [$1] }
    | proc SEMICOLON proc_list                    { $1::$3 }
    ;

proc
    : PROCEDURE ide LP opt_param_list RP opt_dec_list cmd { Proc($2,$4,$6,$7) }
    ;

opt_param_list
    :                                             { [] }
    | param_list                                  { $1 }
    ;

param_list
    : param                                       { [$1] }
    | param COMMA param_list                      { $1::$3 }
    ;

param
    : ide COLON bType                             { Par($1,$3) }
    ;

cmd
    : lexp ASSIGN aexp                            { Ass($1,$3) }
    | BEGIN opt_cmd_list END                      { Blk($2) }
    | IF bexp THEN cmd ELSE cmd                   { Ite($2,$4,$6) }
    | WHILE bexp DO cmd                           { While($2,$4) }
    | FOR ide ASSIGN aexp TO aexp DO cmd          { For($2,$4,$6,$8) }
    | REPEAT cmd UNTIL bexp                       { Repeat($2,$4) }
    | WRITE aexp                                  { Write($2) }
    | CALL ide LP opt_aexp_list RP                { PCall($2,$4) }
    ;

opt_cmd_list
    :                                             { [] }
    | cmd_list                                    { $1 }
    ;

cmd_list
    : cmd                                         { [$1] }
    | cmd SEMICOLON cmd_list                      { $1::$3 }
    ;

lexp
    : ide                                         { LVar($1) }
    | ide LBRACKET aexp RBRACKET                  { LVec($1,$3) }
    ;

bexp_factor
    : TRUE                                        { B(true) }
    | FALSE                                       { B(false) }
    | aexp EQUAL aexp                             { Equ($1,$3) }
    | aexp LESSEQUAL aexp                         { LE($1,$3) }
    | aexp LESS aexp                              { LT($1,$3) }
    | NOT bexp_factor                             { Not($2) }
    | LP bexp RP                                  { $2 }
    ;

bexp_term
    : bexp_term AND bexp_factor                   { And($1,$3) }
    | bexp_factor                                 { $1 }
    ;

bexp
    : bexp OR bexp_term                           { Or($1,$3) }
    | bexp_term                                   { $1 }
    ;
   
rexp_term
	: ide										  { Sref($1) }
    | AT rexp_term								  { Mref($2) }
    
dexp_term
	: ide										  { Sunref($1) }
	| CARET dexp_term							  { Munref($2) }

aexp_factor
    : NAT                                         { N($1) }
    | REAL                                        { R($1) }
    | ide                                         { Ident($1) }
    | ide LBRACKET aexp RBRACKET                  { Vec($1,$3) }
    | LP aexp RP                                  { $2 }
    | AT rexp_term								  { Ref($2) }
    | CARET dexp_term							  { Deref($2) }
    ;

aexp_term
    : aexp_term TIMES aexp_factor                 { Mul($1,$3) }
    | aexp_term DIVISION aexp_factor              { Div($1,$3) }
    | aexp_factor                                 { $1 };
    ;

aexp
    : aexp PLUS aexp_term                         { Sum($1,$3) }
    | aexp MINUS aexp_term                        { Sub($1,$3) }
    | aexp_term                                   { $1 }
    ;
    
opt_aexp_list
    :                                             { [] }
    | aexp_list                                   { $1 }
    ;

aexp_list
    : aexp                                        { [$1] }
    | aexp COMMA aexp_list                        { $1::$3 }
    ;

ide
    : IDE                                         { Ide($1) }
    ;

%%

(* FOOTER *)
