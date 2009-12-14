(* identifiers *)
type ide =
	Ide of string

(* Reference *)
type rexp =
	  Sref 		of ide		(* Simple Reference => @ide *)
	| Mref 		of rexp		(* Long Reference => @@@ide *)

(* Derefence *)
type dexp =
	  Sunref	of ide		(* Simple Derefencete => ^ide *)
	| Munref 	of dexp		(* Multiple Dereference => ^^^ide *)

(* arithmetical expressions *)
type aexp  =
	  N			of int
	| R			of float
	| Ident		of ide
	| Ref		of rexp
	| Deref		of dexp
	| Vec		of ide * aexp
	| Sum		of aexp * aexp
	| Sub		of aexp * aexp
	| Mul		of aexp * aexp
	| Div		of aexp * aexp

(* boolean expressions *)
type bexp =
	  B			of bool
	| Equ		of aexp * aexp
	| LE		of aexp * aexp
	| LT		of aexp * aexp
	| Not		of bexp
	| And		of bexp * bexp
	| Or		of bexp * bexp

(* left expressions *)
type lexp =
	  LVar 		of ide
	| LVec 		of ide * aexp

(* commands *)
type cmd =
	  Ass		of lexp * aexp
	| Blk		of cmd list
	| Ite		of bexp * cmd * cmd
	| While		of bexp * cmd
	| For		of ide * aexp * aexp * cmd
	| Repeat	of cmd * bexp
	| Write		of aexp
	| PCall		of ide * aexp list

(* declarations *)
type bType =
	  Int
	| Float

type pType =
	  SPointer	of bType	(* Last level, pointer basic type *)
	| MPointer	of pType	(* One pType means one indirection level *)

type gType =
	  Basic		of bType
	| Const		of bType * aexp
	| Pointer	of pType
	| Vector	of bType * int * int

type dec =
	  Dec		of ide * gType

(* procedures *)
type param =
	  Par		of ide * bType

type sub_prog =
	  Proc		of ide * param list * dec list * cmd

(* programs *)
type program =
	  Program	of dec list * sub_prog list * cmd
	| Null
