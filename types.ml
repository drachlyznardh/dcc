open Syntaxtree;;

(* Cell addresses *)
type loc =
	  Loc of int					(* Location, position *)
	| Null							(* No location *)

(* Cell values *)
type value =
      ValueInt		of int				(* Type integer with value *)
    | ValueFloat	of float			(* Type float with value *)
    | StoreLoc		of loc				(* Location in the Store space *)
    | HeapLoc		of loc				(* Location in the Heap space *)

(* Environment entries *)
type rentry =
	  Var of bType * loc			(* Variabile with type and residence *)
	| Val of value					(* Constant value *)
	| Descr_Pntr of
		bType * int * loc			(* Pointer with final type, depth and residence *)
	| Descr_Vctr of
		bType * int * int * loc		(* Vector with final type, bounds and residence *)
	| Descr_Prcd of
		param list * dec list * cmd	(* Procedure with parameters, declarations and body *)

(* Heap entries *)
type hentry =
	HEntry of int * value

