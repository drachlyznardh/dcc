(* identifiers *)
type ide =
	Ide of string

(* Derefence *)
type dexp =
	  Sunref	of ide		(* Simple Derefencete => ^ide *)
	| Munref 	of dexp		(* Multiple Dereference => ^^^ide *)

(* Declarations *)

(* Native types *)
type bType =
	  Int
	| Float

(* Pointer types *)
type pType =
	  SPointer	of bType	(* Last level, pointer basic type *)
	| MPointer	of pType	(* One pType means one indirection level *)

(* Global types: because of Malloc's need to know the data type to allocate,
these declaration have to be done together *)
type gType =
	  Basic		of bType
	| Const		of bType * aexp
	| Pointer	of pType
	| Vector	of bType * int * int

(* arithmetical expressions *)
and aexp  =
	  N			of int
	| R			of float
	| Ident		of ide
	| Ref		of ide
	| Unref		of dexp
	| Malloc	of gType
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
	| Lunref	of dexp

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
	| Free		of lexp

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
	| NoProg
