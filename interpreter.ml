(* (2003)Originale ML(Marco Pistore & Floriano Zini) *)
(*    (2004)Estensione OCAML(Fabrizio Lazzarotto)    *)
(*          (2007)Revisione (Stefano Schivo)         *)
(*         (2009)Sconvolgimento(Ivan Simonini)       *)

open Syntaxtree;;
open Types;;
open Exception;;
open Common;;

open Env;;
open Store;;
open Heap;;

(******************************)

(* THE INTERPRETER *)

let initenv =	new env 16		(* Initially empty REnv *)
let initstore = new store 16	(* Initially empty Store *)
let initheap =	new heap 16		(* Initially empty Heap *)

let initall = (initenv,initstore,initheap)

(** START OF FUFFA **)

(* Calculate pointer depth *)
let pntr_depth (p:pType) : int =
	let rec aux p = match p with
		  SPointer(_) -> 1
		| MPointer(next) -> 1 + (aux next)
	in aux p

let pntr_bType (p:pType) : bType =
	let rec aux p = match p with
		  SPointer(v) -> v
		| MPointer(next) -> aux next
	in aux p
	
(* Get final identifier name from a pointer *)
let pntr_get_data (p:dexp) (r:env) : (bType * value * int) =
	let rec aux (p:dexp) (r:env) = match p with
		  Sunref(id) -> (match r#get id with
		  					  Var(b,l) ->			(b,StoreLoc(l), 1)
		  					| Val(v) ->				(match v with
		  												  ValueInt(_) ->	(Int,v,1)
		  												| ValueFloat(_) ->	(Float,v,1)
		  												| _ ->				raise (MY_FAULT "pntr_get_data")
		  											)
		  					| Descr_Pntr(b,_,l) ->	(b,StoreLoc(l),1)
		  					| _ ->					(match id with 
		  												  Ide(name) ->	raise (Not_a_pointer ("pntr_get_data["^name^"]")))
		  				)
		| Munref(next) -> let (b,v,d) = aux next r in (b,v,d+1)
	in aux p r
	
let rec do_deref (depth:int) (v:value) (s:store) (h:heap) : value = 
	if depth == 0 then
		v
	else match v with
		  StoreLoc(l) ->	do_deref (depth - 1) (s#get l) s h
		| HeapLoc(l) ->		do_deref (depth - 1) (h#get l) s h
		| ValueInt(v) ->	raise (SYNTAX ("Do_deref_value: ValueInt("^(string_of_int v)^")is not a ValueLoc"))
		| ValueFloat(v) ->	raise (SYNTAX ("Do_deref_value: ValueFloat("^(string_of_float v)^") is not a ValueLoc"))

(* Where is this identifier's value saved? *)
let get_residence (i:ide) (r:env) :value =
	match r#get i with
		  Var(_,l) ->				StoreLoc(l)
		| Val(_) ->					raise (Resident_evil "It's a Constant!")
		| Descr_Pntr(_,_,l) ->		StoreLoc(l)
		| Descr_Vctr(_,_,_,l) ->	StoreLoc(l)
		| Descr_Prcd(_,_,_) ->		raise (Resident_evil "It's a Procedure!")

let get_residence_vec (i:ide) (off:int) (r:env) :value =
	match r#get i with
		  Descr_Vctr(_,lb,ub,Loc(l)) ->	let real = l - lb + off in
		  									StoreLoc(Loc(real))
		| _ ->							raise (MY_FAULT "get_residence_vec")

(* I need THAT value, I don't mind where is it saved. *)
let get_value (l:value) (s:store) (h:heap) :value = match l with
	  StoreLoc(sl) ->	s#get sl
	| HeapLoc(hl) ->	h#get hl
	| _ -> raise (MY_FAULT "get_value")

(* I need something to get changed: do it and don't bother me *)
let set_value (l:value) (v:value) (s:store) (h:heap) = 
(*	print_string ("\n\t"^(string_of_value l)^"->"^(string_of_value v)); *)
	match l with
	  StoreLoc(sl) ->	s#set sl v
	| HeapLoc(hl) ->	h#set hl v
	| _ -> raise (MY_FAULT "set_value")

let get_bType (l:lexp) (r:env) : bType =
	match l with
		  LVar(id)  ->		r#get_bType id
		| LVec(vid,_) ->	r#get_bType vid
		| Lunref(u) ->		let (b,_,_) = pntr_get_data u r in b

(** END OF FUFFA **)

(* evaluation of declarations *)
let rec decl_eval (d:dec list) (r:env) (s: store) (h:heap) = (
	match d with
		  []                        ->  ()
		| Dec(x,Basic(tipo))::decls ->  let nl = s#newmem in
											(match tipo with
												  Int   ->	r#set x (Var(Int,nl));
															s#set nl (ValueInt(0));
															decl_eval decls r s h
												| Float ->	r#set x (Var(Float,nl));
															s#set nl (ValueFloat(0.0));
															decl_eval decls r s h
											)
		| Dec(x,Const(t,e))::decls ->	let nv = eval_aexp e r s h in
											(match (t,nv) with
												(Int,ValueInt(iv)) ->		r#set x (Val(ValueInt(iv)));
																			decl_eval decls r s h
												| (Float,ValueFloat(fv)) ->	r#set x (Val(ValueFloat(fv)));
																			decl_eval decls r s h
												| (_,_) ->					raise (DIFFERENT_TYPE_ASSIGNATION "decl_eval:Const")
											)
		| Dec(x,Pointer(p))::decls ->	let nl = s#newmem
											and depth = pntr_depth p
											and ptype = pntr_bType p
										in (
											r#set x (Descr_Pntr(ptype,depth,nl));
											s#set nl (StoreLoc(Null));
											decl_eval decls r s h
										)
		| Dec(x,Vector(t,lb,ub))::decls ->	let nl = s#newmem and dim = ub - lb + 1 in
												let vo = get_vo nl lb in
													(match t with
														  Int ->	r#set x (Descr_Vctr(Int,lb,ub,vo));
																	s#setvec (nl) dim (ValueInt(0));
																	decl_eval decls r s h
														| Float ->	r#set x (Descr_Vctr(Float,lb,ub,vo));
																	s#setvec (nl) dim (ValueFloat(0.0));
																	decl_eval decls r s h
													)
)
(* evaluation of arithmetical expressions *)
and eval_aexp (e:aexp) (r:env) (s:store) (h:heap): value = (
	match e with
		  N(n)      ->  ValueInt(n)
		| R(n)      ->  ValueFloat(n)
		| Ident(i)  ->  (match r#get i with
							  Var(_,l) ->			s#get l
							| Val(v) ->				v
							| Descr_Pntr(_,_,l) ->	s#get l
							| _ ->					(match i with 
														Ide(name) -> raise (SYNTAX ("Eval_aexp:Ident: Id not found("^name^")") )
													)
		                )
		| Unref(p)  ->	let (_,l,d) = (pntr_get_data p r) in	(* First I get the unreference last location *)
							do_deref (d + 1) l s h				(* Then I get that final location stored value *)
		| Ref(i)    ->	get_residence i r
		| Malloc(t) ->	(match t with
							  Basic(b) ->			let l = h#newmem in
														(match b with
															  Int -> h#set l (ValueInt(0));
															| Float -> h#set l (ValueFloat(0.0));
														);
														HeapLoc(l)
							| Const(_,_) ->			raise (SYNTAX "You don't want to declare dynamic constant, do you?")
							| Pointer(p) ->			let l = h#newmem in
														(match p with 
															_ -> h#set l (HeapLoc(Null))
														);
														HeapLoc(l)
							| Vector(b,lb,ub) ->	let hm = 1 + ub - lb in
														let fc = h#lnewmem hm in
															let n = mkid fc
															and vo = get_vo fc lb in
																r#set n (Descr_Vctr(b,lb,ub,vo));
																(match b with
																	  Int ->	h#set_vec fc (ValueInt(0)) hm; 
																	  			HeapLoc(fc)
																	| Float ->	h#set_vec fc (ValueFloat(0.0)) hm;
																				HeapLoc(fc)
																);
						)
		| Vec(v,off) ->	let res = get_offset off r s h in
							print_string ("\nEvaluating "^"\n");
							(match r#get v with
								  Descr_Vctr(_,lb,ub,Loc(vo)) ->	if (res >= lb && res <= ub)
												  						then s#get (Loc(vo + res))
																		else raise (INDEX_OUT_OF_BOUNDS "Eval_aexp:Vec:I" )
								| Descr_Pntr(_,_,l) ->
									let home = get_loc (s#get l) in
										(try (
											let desc = r#get (mkid home) in
												(match desc with
													  Descr_Vctr(_,lb,ub,Loc(vo)) ->
													  		if (res >= lb && res <= ub)
													  			then h#get (Loc(vo + res))
													  			else raise (INDEX_OUT_OF_BOUNDS "Eval_aexp:Vec:II")
													| _ -> raise (MY_FAULT "Eval_aexp:Vec")
												)
											) with Env_404(s) ->	raise (Segfault ((get_name v)^"["^(string_of_int res)^"]/"^s))
										)
							| _ -> raise (SYNTAX "Eval_aexp:Vec: That's no descriptor")
						)
		
		| Sum (a,b) ->	aexp_op_fun a b r s h (+) (+.)
		| Sub (a,b) ->	aexp_op_fun a b r s h (-) (-.)
		| Mul (a,b) ->	aexp_op_fun a b r s h ( * ) ( *. )
		| Div (a,b) ->	aexp_op_fun a b r s h (/) (/.)
		| Mod (a,b) ->	aexp_op_fun a b r s h (mod) (mod_float)

) and aexp_op_fun  (a:aexp) (b:aexp) (r:env) (s:store) (h:heap) fi fr = (
	let aValue = (eval_aexp a r s h) and bValue = (eval_aexp b r s h) in
		(match aValue,bValue with 
			  ValueInt(op1),ValueInt(op2) ->		ValueInt(fi op1 op2)
			| ValueFloat(op1),ValueFloat(op2) ->	ValueFloat(fr op1 op2)
			| ValueInt(_),ValueFloat(_) ->			raise (DIFFERENT_TYPE_OPERATION "aexp_op_fun:I")
			| ValueFloat(_),ValueInt(_) ->			raise (DIFFERENT_TYPE_OPERATION "aexp_op_fun:II")
			| _,_ ->								raise (POINTER_ARITHMETIC "aexp_op_fun")
		)

) and get_offset (e:aexp) (r:env) (s:store) (h:heap) :int = (
	let res = eval_aexp e r s h in
		match res with
			  ValueInt(off) ->	off
			| _ ->				raise (NOT_INTEGER_INDEX "get_offset")
)

let rec eval_bexp (e:bexp) (r:env) (s:store) (h:heap) = match e with
      B(b)      ->  b
    | And (a,b) ->  ((eval_bexp a r s h) && (eval_bexp b r s h))
    | Or  (a,b) ->  ((eval_bexp a r s h) || (eval_bexp b r s h))
    | Equ (a,b) ->  ((eval_aexp a r s h)  = (eval_aexp b r s h))
    | LE  (a,b) ->  ((eval_aexp a r s h) <= (eval_aexp b r s h))
    | LT  (a,b) ->  ((eval_aexp a r s h)  < (eval_aexp b r s h))
    | Not (a)   ->  (not(eval_bexp a r s h))

(* declaration of subprograms *)
let rec sub_prog_decl_eval (d: sub_prog list) (r:env) (s:store) (h:heap) =
	match d with
		  [] -> ()
		| Proc(x,params,locals,cmds)::decls ->	r#set x (Descr_Prcd(params,locals,cmds));
												sub_prog_decl_eval decls r s h


(* evaluation of actual parameter list *)
let rec eval_actual_params (e:aexp list) (r:env) (s:store) (h:heap) = match e with
      []        ->  [] 
    | esp::vl   ->  (eval_aexp esp r s h)::(eval_actual_params vl r s h)

(* prepare for execution of subprograms *)
let type_check (input_values:value list) (parameters:param list) = 
    let rec do_check (av:value list) (fp:param list) :bool =
        match (av,fp) with
              [],[] ->						true
            | (v)::acts,Par(id,t)::forms ->
	            	(match v,t with
						  ValueInt(_),Int ->		do_check acts forms
						| ValueFloat(_),Float ->	do_check acts forms
						| _,Int ->					raise (PARAMETERS_DO_NOT_MATCH "type_check:should be int")
						| _,Float ->				raise (PARAMETERS_DO_NOT_MATCH "type_check:should be float")
					)
            | _ ->							raise (SYNTAX "Type_checking: error")
    in
        if (List.length(input_values) != List.length(parameters))
            then false
        	else (do_check input_values parameters)

let rec assign_values (f:param list) (a:value list) (r:env) (s:store) (h:heap) =
    match (f,a) with
          [],[] -> ()
        | Par(id,tipo)::fs,v::acts ->	decl_eval [Dec(id,Basic(tipo))] r s h;
        								assign_values fs acts r s h;
										(match r#get id with
											Var(_,l) ->	s#set l v
											| _ ->		raise (SYNTAX "Assign_values: Not a Variable")
										)
        | _ -> raise (SYNTAX "Assign_values: Not a list")

(* execution of commands *)
let rec exec (c: cmd) (r: env) (s: store) (h:heap) = match c with
      Ass(i,e) ->		let ret = eval_aexp e r s h
      						and b = get_bType i r
      					in
							(match ret,b with
								  ValueInt(_),Int ->		()
								| ValueFloat(_),Float ->	()
								| StoreLoc(_),loc ->		()
								| HeapLoc(_),loc ->			()
								| f,s ->					raise (DIFFERENT_TYPE_OPERATION "Exec:Ass")
							);
							(match i with
								  LVar(id)  ->	let l = get_residence id r in
					  						let oldv = get_value l s h in
												(* Now check for SAGE *)
												(match oldv with
													  HeapLoc(hl) ->	h#do_sage hl r;
													| _ ->				()
												);
						  						(* Now check for BUMP *)
						  						(match ret with
						  							  HeapLoc(hl) ->	h#do_bump hl r;
						  							| _ ->				()
												);
						  						set_value l ret s h;
								| LVec(v,off) ->
										let res = get_offset off r s h in
											(match r#get v with
												  Descr_Vctr(_,lb,ub,Loc(vo)) ->
												  		if (res >= lb && res <= ub)
															then s#set (Loc(vo + res)) ret
															else raise (INDEX_OUT_OF_BOUNDS "exec:Ass:LVec:I")
												| Descr_Pntr(b,d,l) ->
														let home = get_loc (s#get l) in
															(try (
																let desc = r#get (mkid home) in
																	(match desc with
																		  Descr_Vctr(_,lb,ub,Loc(vo)) ->
																				if (res >= lb && res <= ub)
																					then h#set (Loc(vo + res)) ret
																					else raise (INDEX_OUT_OF_BOUNDS "Exec:Ass:LVec:II")
																		| _ -> raise (MY_FAULT "Exec:Ass:LVec")
																	)
																) with Env_404(s) ->
																	raise (Segfault ((get_name v)^"["^(string_of_int res)^"]/"^s))
															)
												| _ -> raise (SYNTAX "Exec:Ass:LVec: That's no descriptor")
											)
								| Lunref(u) ->
										let (_,l,d) = pntr_get_data u r in
											set_value (do_deref d l s h) ret s h
                        	)
    | Blk([]) ->		()
    | Blk(x::y) ->		exec x r s h;
    					exec (Blk(y)) r s h
    | Ite(b,c1,c2) ->	if (eval_bexp b r s h) then (exec c1 r s h)
                        else (exec c2 r s h)
    | While(b,c) ->		if (not(eval_bexp b r s h)) then exec (While(b,c)) r s h
    | For(i,minexp,maxexp,c) ->
						let min = eval_aexp minexp r s h
                        	and update_counter l s = (match (s#get l) with
								  ValueInt(n) ->	s#set l (ValueInt(n + 1))
								| _ ->				raise (NOT_INTEGER_INDEX "Exec:For:update_counter")
							) in 
							(match r#get i with
								  Var(_,l) ->	(s#set l min;											(* Assign min value to counter *)
												let rec exec_for s = (
													exec c r s h;
													if eval_bexp (LT(Ident(i), maxexp)) r s h then (	(* Check condition *)
														update_counter l s;								(* Then update counter *)
														exec_for s										(* Then execute again *)
													)
												) in exec_for s											(* Starting from the beginning *)
												)
								| _ ->			raise (SYNTAX "Exec(For): Not a Variable")
							)
    | Repeat(c,b)   ->  exec c r s h;						(* Execute body command once, then ... *)
						if (not(eval_bexp b r s h))			(* ... until the condition is satisfied ... *)
							then exec (Repeat(c,b)) r s h	(* ... repeat same command one more time *)
    | Write(e)      ->  let ret = (eval_aexp e r s h) in	(* Evaluate the expression, then print its value on screen *)
							(match ret with
								  ValueInt(op1) ->		print_string ("\nInt:\t"^(string_of_int op1))
								| ValueFloat(op1) ->	print_string ("\nFlt:\t"^(string_of_float op1) )
								| StoreLoc(op1) ->		print_string ("\nSLc:\t"^(string_of_loc op1))
								| HeapLoc(op1) ->		print_string ("\nHLc:\t"^(string_of_loc op1))
							)
    | PCall(id,exprs)
                    ->  let input_values = eval_actual_params exprs r s h in
						(match (r#get id) with
							  Descr_Prcd(params,locals,cmds) ->
									if ((type_check input_values params))
										then exec_proc id input_values r s h
										else raise (PARAMETERS_DO_NOT_MATCH "Exec:PCall")
							| _ ->	raise (SYNTAX "Exec(Pcall): Not a Descr_Procedure")
						)
	| Free(p) ->		(let lookforloc (p:lexp) :value = (
							match p with
								  LVar(id) ->		get_value (get_residence id r) s h
								| LVec(id,aexp) ->	(let off = (eval_aexp aexp r s h) in
														match off with
															  ValueInt(v) ->	get_value (get_residence_vec id v r) s h
															| _ ->				raise (INDEX_OUT_OF_BOUNDS "Exec:Free")
													)
								| Lunref(u) ->		let (_,l,d) = pntr_get_data u r in
														get_value (do_deref d l s h) s h
							) in let d = lookforloc p in 
								match d with
									  HeapLoc(l) ->	h#do_free l r
									| _ ->			raise (DIFFERENT_TYPE_OPERATION ("Exec:Free["^(string_of_value d)^"]"))
						)

(* execution of subprograms *)
and exec_proc (id:ide) (input_values:value list) (r:env) (s:store) (h:heap) =
	let do_exec inVars locVars cmds (values:value list) (r:env) (s:store) (h:heap) = (
		r#push (get_name id);				(* Create new nested environment *)
		assign_values inVars values r s h;	(* Assign current values *)
		decl_eval locVars r s h;			(* Declare local variables *)
		r#show;								(* Debug print *)
		exec cmds r s h;					(* Execute procedure body *)
		r#pop								(* Trash the nested environment *)
	) in match r#get id with
		  Descr_Prcd(params,locals,cmds) ->	do_exec params locals cmds input_values r s h
		| _ ->								raise (SYNTAX "Exec_proc: Not a Descr_Procedure")


(* evaluation of programs *)
let run prog = 
    match prog with
		  Program(vars,sub_progs,com) ->	let (r,s,h) = initall in	(* Initiallize all utility structures *)
		  										decl_eval vars r s h;				(* Declare global variables *)
			  									sub_prog_decl_eval sub_progs r s h;	(* Declare all subprograms *)
			  									exec com r s h						(* Execute main program *)
		| NoProg -> ()

