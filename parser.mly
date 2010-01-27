%{ (* HEADER *)

open Syntaxtree;;

%}

%token <string>	IDE
%token <int>	NAT
%token <float>	REAL

%token			TRUE FALSE

%token			PROGRAM PROCEDURE VAR CONST INT FLOAT BEGIN END 
%token			IF THEN ELSE WHILE DO FOR TO REPEAT UNTIL WRITE
%token			ARRAY OF LBRACKET RBRACKET DOTS CALL MALLOC FREE

%token			PLUS MINUS TIMES DIVISION MODULE
%token			EQUAL LESSEQUAL LESS AND OR NOT 
%token			ASSIGN CARET AT

%token			SMCLN COLON COMMA

%token			LP RP

%token			EOF

%start program
%type<Syntaxtree.program> program

%%

program 
    : PROGRAM opt_dec_list opt_proc_list cmd EOF	{ Program($2,$3,$4) }
    ;

opt_dec_list
    :												{ [] }
    | dec_list										{ $1 }
    ;

dec_list
	: dec											{ [$1] }
	| dec dec_list									{ $1::$2 }
	;

dec
	: VAR ide COLON gType SMCLN						{ Dec($2,$4) }
	| CONST ide COLON bType ASSIGN aexp SMCLN		{ Dec($2, Const($4, $6)) }
	;

gType
    : bType											{ Basic($1) }
    | ARRAY LBRACKET NAT DOTS NAT RBRACKET OF bType	{ Vector($8,$3,$5) }
    | pType											{ Pointer($1) }
    ;

pType
	: CARET bType								  { SPointer($2) }
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
    : proc											{ [$1] }
    | proc proc_list								{ $1::$2 }
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
	: lexp ASSIGN aexp SMCLN						{ Ass($1,$3) }
	| BEGIN opt_cmd_list END						{ Blk($2) }
	| IF bexp THEN cmd ELSE cmd						{ Ite($2,$4,$6) }
	| WHILE bexp DO cmd								{ While($2,$4) }
	| FOR ide ASSIGN aexp TO aexp DO cmd			{ For($2,$4,$6,$8) }
	| REPEAT cmd UNTIL bexp SMCLN					{ Repeat($2,$4) }
	| WRITE aexp SMCLN								{ Write($2) }
	| CALL ide LP opt_aexp_list RP SMCLN			{ PCall($2,$4) }
	| FREE LP lexp RP SMCLN							{ Free($3) }
    ;

opt_cmd_list
    :                                             { [] }
    | cmd_list                                    { $1 }
    ;

cmd_list
	: cmd											{ [$1] }
	| cmd cmd_list									{ $1::$2 }
	;

lexp
    : ide                                         { LVar($1) }
    | ide LBRACKET aexp RBRACKET                  { LVec($1,$3) }
    | CARET dexp								  { Lunref($2) }
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
   
dexp
	: ide										  { Sunref($1) }
	| CARET dexp								  { Munref($2) }

aexp_factor
    : NAT                                         { N($1) }
    | REAL                                        { R($1) }
    | ide                                         { Ident($1) }
    | ide LBRACKET aexp RBRACKET                  { Vec($1,$3) }
    | LP aexp RP                                  { $2 }
    | AT ide									  { Ref($2) }
    | CARET dexp								  { Unref($2) }
    ;

aexp_term
    : aexp_term TIMES aexp_factor					{ Mul($1,$3) }
    | aexp_term DIVISION aexp_factor				{ Div($1,$3) }
    | aexp_term MODULE aexp_factor					{ Mod($1,$3) }
    | aexp_factor									{ $1 };
    ;

aexp
    : aexp PLUS aexp_term                         { Sum($1,$3) }
    | aexp MINUS aexp_term                        { Sub($1,$3) }
    | aexp_term                                   { $1 }
    | MALLOC LP gType RP						  { Malloc($3) }
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
