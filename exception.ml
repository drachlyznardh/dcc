(* Exception *)
exception Resident_evil		of string	(* Constant and Procedure don't have a residence cell in Store *)

(* Not found exception *)
exception Env_404			of string	(* Environment entry not found *)
exception Store_404			of string	(* Store entry not found *)
exception Heap_404			of string	(* Heap entry not found *)

exception Syntax						of string

(* Indexes *)
exception Index_out_of_bounds			of string
exception Not_integer_index				of string

(* Do not mess with different types *)
exception Different_type_operation		of string
exception Different_type_assignation	of string
exception Different_type_pointer		of string

exception Pointer_arithmetic			of string
exception Parameters_do_not_match		of string

(* Pointer exceptions *)
exception Not_a_pointer		of string	(* While calculating pointer's depth *)
exception Null_pointer		of string	(* I herd you like Null *)
exception Double_free		of string	(* Oh, so many times, always unexpected *)
exception Segfault			of string	(* What are you pointing at? *)

(* When it is really my fault, I admit it *)
exception My_fault						of string

