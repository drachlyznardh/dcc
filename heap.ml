open Syntaxtree;;
open Types;;
open Exception;;
open Common;;

open Env;;

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
		) with Not_found -> raise (Heap_404 ("heap#get["^(string_of_loc l)^"]"))
	)
	
	method get_count (l:loc) = (
		try (
			let h = Hashtbl.find htbl l in
				match h with
					HEntry(c,_) -> c
		) with Not_found -> raise (Heap_404 ("heap#get_count["^(string_of_loc l)^"]"))
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
					| _ ->											raise (Different_type_assignation "heap#set")
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
					| _ ->						raise (My_fault "do_bump")
		) with Env_404(s) ->	self#bump l
	)
	
	(* Decrease counter for an HEntry: if 0, remove it *)
	method sage (l:loc):int = (
		check_loc l "sage";
		try (
			let h = Hashtbl.find htbl l in (
				match h with
					HEntry(c,v) ->
						if c == 1 then (
							Hashtbl.remove htbl l; 0						(* Remove entry, deallocation *)
						) else (
							let nc = c - 1 in
								Hashtbl.replace htbl l (HEntry(c - 1,v));	(* Decrease counter *)
								nc
						)
			)
		) with Not_found -> raise (Double_free "heap#sage")
	)
	
	method sage_vec (l:loc) (hm:int) :int = (
		match hm with
			  0 ->	-1
			| 1 ->	self#sage l
			| n ->	ignore (self#sage l);
					self#sage_vec (nextloc l) (hm - 1)
	)
	
	method do_sage (l:loc) (r:env) = (
		let id = mkid l in
			try (
				let re = r#get id in
					match re with
						  Descr_Vctr(b,lb,ub,vl) ->	let dim = ub - lb + 1 in
						  								if self#sage_vec vl dim == 0
						  									then r#remove id
						| _ ->						raise (My_fault "do_sage")
			) with Env_404(s) ->	ignore (self#sage l)
	)
	
	method free (l:loc) = (
		check_loc l "free";
		try (
			Hashtbl.remove htbl l
		) with Not_found -> raise (Double_free "heap#free")
	)
	
	method free_vec (l:loc) (hm:int) = (
		if hm > 0 then (
			self#free l;
			self#free_vec (nextloc l) (hm - 1)
		)
	)
	
	method do_free (l:loc) (r:env) = (
		check_loc l "do_free";
		let id = mkid l in
			try (
				let re = r#get id in
					match re with
						  Descr_Vctr(b,lb,ub,vl) ->	let dim = ub - lb + 1 in
						  								r#remove id;			(* Destroy descriptor *)
						  								self#free_vec vl dim	(* Free each cell *)
						| _ ->						raise (My_fault "do_free")
			) with Env_404(s) ->	self#free l
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
		if self#isfree f
			then if left == 0
				then (true, f)							(* Done: this cell is the last one *)
				else self#arefree (f + 1) (left - 1)	(* Keep on looking on next cell *)
		else (false, f+1)
	)

	method newmem :loc = (
		let rec keepsearching (n:int) = (
			if self#isfree n
				then Loc(n)
				else keepsearching (n + 1)
		) in keepsearching 0
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
				| (Null,_) ->				raise (Null_pointer "Heap#show")
		) in
			print_string "\nHeap:";
			let length = Hashtbl.length htbl in
				if length = 0
					then print_string "\tEmpty\n"
					else Hashtbl.iter lookat htbl 
	)

end;;

