open Syntaxtree;;
open Types;;
open Exception;;

(* Get location value from StoreLoc and HeapLoc *)
let get_loc (v:value) : loc =
	match v with
		  StoreLoc(l) ->	l
		| HeapLoc(l) ->		l
		| _ ->				raise (Not_a_pointer "get_loc: not a location")

(* Get next location *)
let nextloc (l:loc) : loc = 
	match l with
		  Loc(value) ->	Loc(value + 1)
		| Null ->		raise (Null_pointer "nextloc")

let get_vo (l:loc) (off:int) =
	match l with
		  Loc(lv) ->	Loc(lv - off)
		| Null ->		raise (Null_pointer "get_vo")

let print_loc (l:loc) =
	match l with
		  Loc(v) ->	print_int v
		| Null ->	print_string "Null"

let string_of_loc (l:loc) =
	match l with
		  Loc(v) ->	string_of_int v
		| Null ->	"Null"
	
let string_of_type (t:bType) =
	match t with
		  Int ->	"Int"
		| Float ->	"Float"

let string_of_value (v:value) =
	match v with
		  ValueInt(i) -> 	"Int["^(string_of_int i)^"]"
		| ValueFloat(f) ->	"Flt["^(string_of_float f)^"]"
		| StoreLoc(sl) -> 	"SLc["^(string_of_loc sl)^"]"
		| HeapLoc(hl) -> 	"HLc["^(string_of_loc hl)^"]"

(* Extract name form identifier *)
let get_name (i:ide) =
	match i with
		  Ide(name) ->	name

let check_loc (l:loc) (s:string) =
	match l with 
		  Null ->	raise (Null_pointer s)
		| Loc(_) ->	()

(* Dynamic array names *)
let mkloc (i:ide) : loc = match i with
	  Ide(n) ->	Loc(int_of_string n)

let mkid (l:loc) : ide = match l with
	  Loc(n) ->	Ide(string_of_int n)
	| Null ->	raise (Null_pointer ("mkid"))

