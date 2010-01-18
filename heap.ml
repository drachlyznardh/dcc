open Syntaxtree;;

(* memory *)
type loc =
	  Loc of int					(* Location, position *)
	| Null							(* No location *)

type value =
      ValueInt of int				(* Type integer with value *)
    | ValueFloat of float			(* Type float with value *)
    | StoreLoc of loc				(* Location in the Store space *)
    | HeapLoc of loc				(* Location in the Heap space *)

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
exception DEREF_ON_NOT_A_POINTER	of string	(* Are you dereferencing a pointer? Or maybe not? *)
exception NULL_POINTER_EXCEPTION	of string
exception DOUBLE_FREE

(* Get location value from StoreLoc and HeapLoc *)
let get_loc (v:value) : loc = match v with
	  StoreLoc(l) -> l
	| HeapLoc(l) -> l
	| _ -> raise (SYNTAX "Not a location")

(* Get next location *)
let nextloc (l:loc) : loc = 
	match l with
		  Loc(value) -> Loc(value + 1)
		| Null -> raise (NULL_POINTER_EXCEPTION "nextloc")

let print_loc (l:loc) = match l with
	  Loc(v) -> print_int v
	| Null -> print_string "Null"

let string_of_loc (l:loc) = match l with
	  Loc(v) -> string_of_int v
	| Null -> "Null"

let string_of_value (v:value) = match v with
	  ValueInt(i) -> 	"Int"^(string_of_int i)
	| ValueFloat(f) ->	"Flt"^(string_of_float f)
	| StoreLoc(sl) -> 	"SLc"^(string_of_loc sl)
	| HeapLoc(hl) -> 	"HLc"^(string_of_loc hl)

let check_loc (l:loc) (s:string) = match l with Null -> raise (NULL_POINTER_EXCEPTION s) | Loc(_) -> ();

(* Heap class *)
class heap size = object (self)
	
	val mutable newcell = 0
	val mutable htbl = (Hashtbl.create size : (loc, hentry) Hashtbl.t)
	
	method get (l:loc) = Hashtbl.find htbl l
	method get_value (l:loc) = let h = Hashtbl.find htbl l in match h with HEntry(_,v) -> v
	method get_count (l:loc) = let h = Hashtbl.find htbl l in match h with HEntry(c,_) -> c
	
	method set_value (l:loc) (v:value) = (
		check_loc l "set_value";
		try (
			let h = Hashtbl.find htbl l in (
				match (v,h) with
					  (ValueInt(_),HEntry(c,ValueInt(_))) -> Hashtbl.remove htbl l; Hashtbl.replace htbl l (HEntry(c,v))
					| (ValueFloat(_),HEntry(c,ValueFloat(_))) -> Hashtbl.remove htbl l; Hashtbl.replace htbl l (HEntry(c,v))
					| _ -> raise DIFFERENT_TYPE_ASSIGNATION
			)
		) with Not_found -> Hashtbl.replace htbl l (HEntry(1,v))
	)
	
	(* Increase counter for an HEntry *)
	method bump (l:loc) = (
		check_loc l "bump";
		try (
			let HEntry(c,v) = Hashtbl.find htbl l in Hashtbl.remove htbl l; Hashtbl.replace htbl l (HEntry(c + 1,v))
		) with Not_found -> (); self#show
	)
	
	(* Decrease counter for an HEntry: if 0, remove it *)
	method sage (l:loc) = (
		check_loc l "sage";
		try (
			let h = Hashtbl.find htbl l in (
				match h with
					HEntry(c,v) ->
						if c == 1 then (Hashtbl.remove htbl l; self#show)
						else Hashtbl.replace htbl l (HEntry(c - 1,v)); self#show
			)
		) with Not_found -> raise DOUBLE_FREE
	)
	
	method newmem size = (
		let res = Loc(newcell) in
			newcell <- newcell + size; res
	)
	
	method show = (
		let lookat (l:loc) (h:hentry) = (
			print_string "\t";
			match (l,h) with
				(Loc(addr),HEntry(count,value)) -> (
					print_int addr; print_string ":"; print_int count;
					(match value with
						  StoreLoc(v) -> print_string ":slc["; print_loc v;
						| HeapLoc(v) -> print_string ":hlc["; print_loc v;
						| ValueInt(v) -> print_string ":int["; print_int v;
						| ValueFloat(v) -> print_string ":flt["; print_float v;
					);
					print_string "]\n"
				)
				| (Null,_) -> raise (NULL_POINTER_EXCEPTION "show")
		) in print_string "Heap:\n"; let length = Hashtbl.length htbl in if length = 0 then print_string "\tEmpty\n" else Hashtbl.iter lookat htbl 
	)

end;;
