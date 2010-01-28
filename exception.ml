(* Exception *)
exception Resident_evil		of string	(* Constant and Procedure don't have a residence cell in Store *)

(* Not found exception *)
exception Env_404			of string	(* Environment entry not found *)
exception Store_404			of string	(* Store entry not found *)
exception Heap_404			of string	(* Heap entry not found *)

exception SYNTAX						of string

(* Indexes *)
exception INDEX_OUT_OF_BOUNDS			of string
exception NOT_INTEGER_INDEX				of string

(* Do not mess with different types *)
exception DIFFERENT_TYPE_OPERATION		of string
exception DIFFERENT_TYPE_ASSIGNATION	of string
exception DIFFERENT_TYPE_POINTER		of string

exception POINTER_ARITHMETIC			of string
exception PARAMETERS_DO_NOT_MATCH		of string

exception NOT_YET_IMPLEMENTED 			of string	(* LOL, still to be done... *)

(* Pointer exceptions *)
exception Not_a_pointer		of string	(* While calculating pointer's depth *)
exception Null_pointer		of string	(* I herd you like Null *)
exception Double_free		of string	(* Oh, so many times, always unexpected *)
exception Segfault			of string	(* What are you pointing at? *)

(* When it is really my fault, I admit it *)
exception MY_FAULT						of string

