open Syntaxtree;;

(* memory *)
type loc =
	  SLoc of int					(* Store location *)
	| HLoc of int					(* Heap location *)

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

type hentry =
	HEntry of int * value

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
exception NO_SUCH_HEAP_ENTRY		(* Heap entry not found *)
exception DEREF_ON_NOT_A_POINTER	of string	(* Are you dereferencing a pointer? *)
exception LOL_DUNNO

let nextloc (l:loc) : loc = 
	match l with 
		  SLoc(value) -> SLoc(value + 1)
		| HLoc(value) -> HLoc(value + 1)

(* Heap class *)
class heap size = object (self)
	
	val mutable newcell = 0
	val mutable htbl = (Hashtbl.create size : (loc, hentry) Hashtbl.t)
	
	method update (l:loc) (nv:value) = (
		try (
			let ov = Hashtbl.find htbl l in
				match (nv,ov) with
					  (ValueInt(_),HEntry(c,ValueInt(_))) -> Hashtbl.replace htbl l (HEntry(c,nv))
					| (ValueFloat(_),HEntry(c,ValueFloat(_))) -> Hashtbl.replace htbl l (HEntry(c,nv))
					| _ -> raise DIFFERENT_TYPE_ASSIGNATION
		) with Not_found -> Hashtbl.replace htbl l (HEntry(1,nv))
	)

	method get (l:loc) = Hashtbl.find htbl l
	
	method delete (l:loc) = Hashtbl.remove htbl l
	
	(* Increase counter for an HEntry *)
	method bump (l:loc) = (
		try (
			let h = Hashtbl.find htbl l in (
				match h with
					HEntry(c,v) -> Hashtbl.replace htbl l (HEntry(c + 1,v))
			)
		) with Not_found -> ()
	)
	
	(* Decrease counter for an HEntry: if 0, remove it *)
	method sage (l:loc) = (
		try (
			let h = Hashtbl.find htbl l in (
				match h with
					HEntry(c,v) ->
						if c = 0 then Hashtbl.remove htbl l
						else Hashtbl.replace htbl l (HEntry(c - 1,v))
			)
		) with Not_found -> ()
	)
	
	method newmem size = (
		let res = HLoc(newcell) in
			newcell <- newcell + size; res
	)
	
	method show = (
		let lookat (l:loc) (h:hentry) = (
			print_string "\t";
			(match (l,h) with
				  (HLoc(addr),HEntry(count,value)) -> (
					print_int addr; print_string ":"; print_int count;
						(match value with
							  ValueLoc(HLoc(v)) ->	print_string ":hlc["; print_int v;
							| ValueLoc(SLoc(v)) ->	print_string ":slc["; print_int v;
							| ValueInt(v) ->		print_string ":int["; print_int v;
							| ValueFloat(v) ->		print_string ":flt["; print_float v;
						);
					print_string "]\n"
					)
				| (_,_) -> raise (SYNTAX "Not a HLoc in my heap...")
			);
		) in print_string "Heap:\n"; let length = Hashtbl.length htbl in if length = 0 then print_string "\tEmpty\n" else Hashtbl.iter lookat htbl 
	)

end;;
