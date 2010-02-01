open Syntaxtree;;
open Types;;
open Exception;;
open Common;;

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

