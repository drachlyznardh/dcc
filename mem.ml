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

type rentry =
	  Var of bType * loc			(* Variabile with type and residence *)
	| Val of value					(* Constant value *)
	| Descr_Pntr of
		bType * int * loc			(* Pointer with final type, depth and residence *)
	| Descr_Vctr of
		bType * int * int * loc		(* Vector with final type, bounds and residence *)
	| Descr_Prcd of
		param list * dec list * cmd	(* Procedure with parameters, declarations and body *)

type hentry =
	HEntry of int * value

(* Heap entries *)

(* exception *)
exception NO_MEM						of string
exception NO_IDE						of string
exception RESIDENT_EVIL					of string (* Constant and Procedure don't have a residence cell in Store *)
exception SYNTAX						of string
exception INDEX_OUT_OF_BOUNDS			of string
exception DIFFERENT_TYPE_OPERATION		of string
exception DIFFERENT_TYPE_ASSIGNATION	of string
exception DIFFERENT_TYPE_POINTER		of string
exception POINTER_ARITHMETIC			of string
exception PARAMETERS_DO_NOT_MATCH		of string

exception NOT_YET_IMPLEMENTED 			of string	(* LOL, still to be done... *)
exception NOT_A_POINTER					of string	(* While calculating pointer's depth *)
exception NO_SUCH_HEAP_ENTRY			of string	(* Heap entry not found *)
exception DEREF_ON_NOT_A_POINTER		of string	(* Are you dereferencing a pointer? Or maybe not? *)
exception NULL_POINTER_EXCEPTION		of string
exception DOUBLE_FREE					of string
exception MY_FAULT						of string	(* Error due to interpreter malfunction *)

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

let moveloc (l:loc) (v:int) =
	match l with
		  Loc(lv) -> Loc(lv + v)
		| Null -> raise (NULL_POINTER_EXCEPTION "moveloc")

let print_loc (l:loc) = match l with
	  Loc(v) -> print_int v
	| Null -> print_string "Null"

let string_of_loc (l:loc) = match l with
	  Loc(v) -> string_of_int v
	| Null -> "Null"
	
let string_of_type (t:bType) = match t with
	  Int -> "Int"
	| Float -> "Float"

let string_of_value (v:value) = match v with
	  ValueInt(i) -> 	"Int["^(string_of_int i)^"]"
	| ValueFloat(f) ->	"Flt["^(string_of_float f)^"]"
	| StoreLoc(sl) -> 	"SLc["^(string_of_loc sl)^"]"
	| HeapLoc(hl) -> 	"HLc["^(string_of_loc hl)^"]"

let print_value (v:value) = print_string (string_of_value v)

let check_loc (l:loc) (s:string) = match l with Null -> raise (NULL_POINTER_EXCEPTION s) | Loc(_) -> ()

(* Dynamic array names *)
let mkloc (i:ide) : loc = match i with
	  Ide(n) ->	Loc(int_of_string n)

let mkid (l:loc) : ide = match l with
	  Loc(n) ->	Ide(string_of_int n)
	| Null ->	raise (NULL_POINTER_EXCEPTION ("mkid"))

(* Heap class *)
class env size = object (self)

	val mutable rtbl = [("base", (Hashtbl.create size : (ide, rentry) Hashtbl.t))]
	
	method push (name:string) = (
		let nt = (Hashtbl.create 4 : (ide,rentry) Hashtbl.t) in
			rtbl <- (name,nt)::rtbl
	)
	
	method pop = (
		match rtbl with
			  [] ->				();
			| (n,head)::tail ->	Hashtbl.clear head; rtbl <- tail
	)
	
	method set (i:ide) (r:rentry) = (
		match rtbl with
			  [] ->				raise (NO_IDE "env#set")
			| (n,head)::tail ->	Hashtbl.replace head i r; (*self#show*)
	)
	
	method get (i:ide) = (
		let rec subget (tbl:(string * (ide,rentry) Hashtbl.t) list) (i:ide) = (
			match tbl with
				  [] ->				raise Not_found
				| (n,head)::tail ->	(try (Hashtbl.find head i)
										with Not_found -> subget tail i)
		) in subget rtbl i
	)
	
	method show = (
		let rec showtbl (t:(string * (ide,rentry) Hashtbl.t) list) = (
			let looktbl (n:string) (tbl:(ide,rentry) Hashtbl.t) = (
				let lookat (i:ide) (r:rentry) = (
					(match i with
						Ide(name) -> print_string ("\n\t\t["^name^"]:");
					);
					(match r with
						  Var(b,l) ->				print_string ("Var:"^(string_of_type b)^"->"^(string_of_loc l))
						| Val(_) ->					print_string ("Val")
						| Descr_Pntr(b,d,l) ->		print_string ("Pntr:"^(string_of_type b)^(string_of_int d)^"->"^(string_of_loc l))
						| Descr_Vctr(b,lb,ub,l) ->	print_string ("Vctr:"^(string_of_type b)^"->");
													print_string ((string_of_loc l)^":"^(string_of_int lb)^":"^(string_of_int ub))
						| Descr_Prcd(_,_,_) ->		print_string ("Prcd")
					)
				) and length = Hashtbl.length tbl in
					print_string ("\n\t"^n);
					if length == 0 then print_string "[Empty]"
					else Hashtbl.iter lookat tbl
			) in match t with
				  [] -> ();
				| (n,head)::tail -> looktbl n head; showtbl tail
		) in print_string ("\nEnv:"); showtbl rtbl
	)

end;;

(* Store class *)
class store size = object (self)

	val mutable newcell = -1
	val mutable stbl = (Hashtbl.create size : (loc, value) Hashtbl.t)

	method newmem = (
		newcell <- newcell + 1;
		Loc(newcell)
	)

	method get (l:loc) = (
		check_loc l "Store#get";
		Hashtbl.find stbl l
	)

	method set (l:loc) (v:value) = (
		check_loc l "Store#update";
		Hashtbl.remove stbl l; Hashtbl.replace stbl l v;
		(*self#show*)
	)

	method setvec (l:loc) (s:int) (v:value) = (
		match s with
			  1 ->	self#set l v
			| n ->	self#set l v;
					let nl = self#newmem in
						self#setvec nl (s - 1) v  
	)

	method show = (
		let lookat (l:loc) (v:value) = (
			print_string "\n\t\t";
			match l with
				  Loc(lv) ->	print_string ((string_of_int lv)^":"^(string_of_value v))
				| Null ->		print_string ("Null:"^(string_of_value v))
		) in
			print_string "\nStore:";
			let length = Hashtbl.length stbl in
				if length = 0 then print_string "\tEmpty\n"
				else Hashtbl.iter lookat stbl 
	)

end;;

(* Heap class *)
class heap size = object (self)
	
	val mutable newcell = -1
	val mutable htbl = (Hashtbl.create size : (loc, hentry) Hashtbl.t)
	
	method get (l:loc) = (
		check_loc l "heap#get";
		try (
			let h = Hashtbl.find htbl l in
				match h with
					HEntry(_,v) ->	v
		) with Not_found -> raise (NO_SUCH_HEAP_ENTRY ("heap#get["^(string_of_loc l)^"]"))
	)
	
	method get_count (l:loc) = (
		try (
			let h = Hashtbl.find htbl l in
				match h with
					HEntry(c,_) -> c
		) with Not_found -> raise (NO_SUCH_HEAP_ENTRY ("heap#get_count["^(string_of_loc l)^"]"))
	)
	
	method set (l:loc) (v:value) = (
		check_loc l "heap#set";
		try (
			let h = Hashtbl.find htbl l in (
				match (v,h) with
					  (ValueInt(_),HEntry(c,ValueInt(_))) ->		Hashtbl.remove htbl l;
					  												Hashtbl.replace htbl l (HEntry(c,v))
					| (ValueFloat(_),HEntry(c,ValueFloat(_))) ->	Hashtbl.remove htbl l;
																	Hashtbl.replace htbl l (HEntry(c,v))
					| _ ->											raise (DIFFERENT_TYPE_ASSIGNATION "heap#set")
			)
		) with Not_found -> Hashtbl.replace htbl l (HEntry(0,v));
	)
	
	method set_vec (l:loc) (v:value) (hm:int) = (
		match hm with
			  0 ->	()
			| n ->	self#set l v;
					self#set_vec (nextloc l) v (hm - 1)
	)
	
	(* Increase counter for an HEntry *)
	method bump (l:loc) = (
		check_loc l "bump";
		try (
			let HEntry(c,v) = Hashtbl.find htbl l in
				Hashtbl.remove htbl l;
				Hashtbl.replace htbl l (HEntry(c + 1,v))
		) with Not_found -> ();
	)
	
	method bump_vec (l:loc) (hm:int) = (
		if hm > 0 then (
			self#bump l;
			self#bump_vec (nextloc l) (hm - 1)
		)
	)
	
	method do_bump (l:loc) (r:env) = (
		try (
			let re = r#get (mkid l) in
				match re with
					  Descr_Vctr(b,lb,ub,vl) ->	let dim = ub - lb + 1 in
					  								self#bump_vec vl dim;
					| _ ->						raise (MY_FAULT "do_bump")
		) with Not_found ->	self#bump l
	)
	
	(* Decrease counter for an HEntry: if 0, remove it *)
	method sage (l:loc) = (
		check_loc l "sage";
		try (
			let h = Hashtbl.find htbl l in (
				match h with
					HEntry(c,v) ->
						if c == 1 then Hashtbl.remove htbl l
						else Hashtbl.replace htbl l (HEntry(c - 1,v))
			)
		) with Not_found -> raise (DOUBLE_FREE "heap#sage")
	)
	
	method sage_vec (l:loc) (hm:int) = (
		if hm > 0 then (
			self#sage l;
			self#sage_vec (nextloc l) (hm - 1)
		)
	)
	
	method do_sage (l:loc) (r:env) = (	
		try (
			let re = r#get (mkid l) in
				match re with
					  Descr_Vctr(b,lb,ub,vl) ->	let dim = ub - lb + 1 in
					  								self#sage_vec vl dim
					| _ ->						raise (MY_FAULT "do_sage")
		) with Not_found ->	self#sage l
	)
	
	method free (l:loc) = (
		check_loc l "free";
		try (
			Hashtbl.remove htbl l
		) with Not_found -> raise (DOUBLE_FREE "heap#free")
	)
	
	method isfree (i:int) = (
		try (
			let l = Loc(i) in
				let h = Hashtbl.find htbl l in
					match h with
						HEntry(c,v) ->	if c == 0 then (
											Hashtbl.remove htbl l;	(* Destroy the non-referenced cell *)
											true					(* Cell is now free *)
										) else false				(* Cell is refered, can't override *)
		) with Not_found -> true									(* Cell is already free *)
	)
	
	method arefree (f:int) (left:int) = (
		if self#isfree f then
			if left == 0 then
				(true, f)							(* Done: this cell is the last one *)
			else
				self#arefree (f + 1) (left - 1)		(* Keep on looking on next cell *)
		else (false, f+1)
	)

	method newmem :loc = (
		let rec keepsearching (n:int) =
			if self#isfree n then Loc(n)
			else keepsearching (n + 1)
		in keepsearching 0
	)

	method lnewmem (hm:int) :loc = (
		let rec keepsearching (f:int) (left:int) = (
			let res = self#arefree f left in
				match res with
					  (true,fc) ->	Loc(fc - hm)				(* Mission accomplished: the first cell  *)
					| (false,fc) ->	keepsearching (fc + 1) hm	(* Need to restart the quest, from the next cell *)
		) in keepsearching 0 hm
	)
	
	method show = (
		let lookat (l:loc) (h:hentry) = (
			print_string "\n\t\t";
			match (l,h) with
				  (Loc(lv),HEntry(c,v)) ->	print_string ((string_of_int lv)^":"^(string_of_int c)^":"^(string_of_value v))
				| (Null,_) ->				raise (NULL_POINTER_EXCEPTION "Heap#show")
		) in
			print_string "\nHeap:";
			let length = Hashtbl.length htbl in
				if length = 0 then print_string "\tEmpty\n"
				else Hashtbl.iter lookat htbl 
	)

end;;
