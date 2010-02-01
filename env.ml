open Syntaxtree;;
open Types;;
open Exception;;
open Common;;

(* Heap class *)
class env size = object (self)

	val mutable rtbl = [("base", (Hashtbl.create size : (ide, rentry) Hashtbl.t))]
	
	method push (name:string) = (
		let nt = (Hashtbl.create 4 : (ide,rentry) Hashtbl.t) in
			rtbl <- (name,nt)::rtbl
	)
	
	method pop = (
		match rtbl with
			  [] ->				raise (My_fault "env#pop: No base table")
			| (n,head)::tail ->	Hashtbl.clear head; rtbl <- tail
	)
	
	method set (i:ide) (r:rentry) = (
		match rtbl with
			  [] ->				raise (My_fault "env#set: No base table")
			| (n,head)::tail ->	Hashtbl.replace head i r; (*self#show*)
	)
	
	method get (i:ide) = (
		let rec subget (tbl:(string * (ide,rentry) Hashtbl.t) list) (i:ide) = (
			match tbl with
				  [] ->				raise (Env_404 ("env#get["^(get_name i)^"]"))
				| (n,head)::tail ->	(try (Hashtbl.find head i)
										with Not_found -> subget tail i)
		) in subget rtbl i
	)
	
	(* Used to remove dynamic vector descriptors *)
	method remove (i:ide) = (
		let rec subremove (tbl:(string * (ide,rentry) Hashtbl.t) list) (i:ide) = (
			match tbl with
				  [] ->				()
				| (n,head)::tail ->	Hashtbl.remove head i;	(* Remove the descriptor, if exists *)
									subremove tail i		(* Try to remove from the any higher scope *)
		) in subremove rtbl i
	)
	
	method get_bType (i:ide) = (
		let re = self#get i in
			match re with
				  Var(b,_) ->				b
				| Val(v) ->					(match v with
	  											  ValueInt(_) ->	Int
	  											| ValueFloat(_) ->	Float
	  											| _ ->				raise (My_fault "env#get_bType: const")
	  										)
				| Descr_Pntr(b,_,_) ->		b
				| Descr_Vctr(b,_,_,_) ->	b
				| Descr_Prcd(_) ->			raise (My_fault "get_bType")
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


