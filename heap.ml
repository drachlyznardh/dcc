open Syntaxtree;;

(* memory *)
type loc =
	Loc of int						(* Location, position *)

type value =
      ValueInt of int				(* Type integer with value *)
    | ValueFloat of float			(* Type float with value *)
    | ValueLoc of loc				(* Type location with value *)

type store =
	loc -> value					(* Store: location to simple values *)

type env_entry =
	  Var of loc					(* Location, variable *)
	| Val of value					(* Value, constant *)
	| Descr_Pntr of int * loc		(* Pointer with depth (1+) and location *)
	| Descr_Vector of
		loc * int * int				(* Vector with start point, lower and upper bounds *)
	| Descr_Procedure of
		param list * dec list * cmd	(* Procedure descriptor *)

type env =
	ide -> env_entry				(* Environment entries *)

type hloc =
	HLoc			of loc

type hentry =
	  HEntry	of value		(* End of chain, simple value *)

(* Heap entries *)

(* exception *)
exception NO_MEM
exception NO_IDE
exception SYNTAX					of string
exception INDEX_OUT_OF_BOUNDS
exception DIFFERENT_TYPE_OPERATION
exception DIFFERENT_TYPE_ASSIGNATION
exception DIFFERENT_TYPE_POINTER
exception PARAMETERS_DO_NOT_MATCH

exception NOT_YET_IMPLEMENTED 		of string	(* LOL, still to be done... *)
exception NOT_A_POINTER				(* While calculating pointer's depth *)
exception NO_HEAP					(* Heap non existent *)
exception NO_SUCH_HEAP_ENTRY		(* Heap entry not found *)
exception DEREF_ON_NOT_A_POINTER	of string	(* Are you dereferencing a pointer? *)
exception LOL_DUNNO

(* Heap class *)
class heap size = object (self)
	
	val mutable newcell = -1
	val mutable htbl = (Hashtbl.create size : (loc, value) Hashtbl.t)
	
	method update (l:loc) (nv:value) = (
		try (
			let ov = Hashtbl.find htbl l in
				match (nv,ov) with
					  (ValueInt(_),ValueInt(_)) -> Hashtbl.replace htbl l nv
					| (ValueFloat(_),ValueFloat(_)) -> Hashtbl.replace htbl l nv
					| _ -> raise DIFFERENT_TYPE_ASSIGNATION
		) with Not_found -> Hashtbl.replace htbl l nv
	)

	method get (l:loc) = Hashtbl.find htbl l

end;;
