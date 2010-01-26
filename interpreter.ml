(* (2003)Originale ML(Marco Pistore & Floriano Zini) *)
(*    (2004)Estensione OCAML(Fabrizio Lazzarotto)    *)
(*          (2007)Revisione (Stefano Schivo)         *)
(*         (2009)Sconvolgimento(Ivan Simonini)       *)

open Syntaxtree;;
open Mem;;

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

let pntr_finaltype (p:pType) : bType =
	let rec aux p = match p with
		  SPointer(v) -> v
		| MPointer(next) -> aux next
	in aux p

(* Get final identifier name from a pointer *)
let pntr_get_data (p:dexp) (r:env) : (value * int) =
	let rec aux (p:dexp) (r:env) = match p with
		  Sunref(id) -> (match r#get id with
		  					  Var(_,l) -> (StoreLoc(l), 1)
		  					| Val(v) -> (v,1)
		  					| Descr_Pntr(_,_,l) -> (StoreLoc(l),1)
		  					| _ -> (match id with Ide(name) -> raise (NOT_A_POINTER ("pntr_get_data["^name^"]")))
		  				)
		| Munref(next) -> let (a,b) = aux next r in (a, b+1)
	in aux p r
	
let rec do_deref (depth:int) (v:value) (s:store) (h:heap) : value = 
	if depth == 0 then
		v
	else match v with
		  StoreLoc(l) -> do_deref (depth - 1) (s#get l) s h
		| HeapLoc(l) -> do_deref (depth - 1) (h#get l) s h
		| ValueInt(v) -> raise (SYNTAX ("Do_deref_value: ValueInt("^(string_of_int v)^")is not a ValueLoc"))
		| ValueFloat(v) -> raise (SYNTAX ("Do_deref_value: ValueFloat("^(string_of_float v)^") is not a ValueLoc"))

(* Where is this identifier's value saved? *)
let get_residence (i:ide) (r:env) :value = match r#get i with
	  Var(_,l) ->				StoreLoc(l)
	| Val(_) ->					raise (RESIDENT_EVIL "It's a Constant!")
	| Descr_Pntr(_,_,l) ->		StoreLoc(l)
	| Descr_Vctr(_,_,_,l) ->	StoreLoc(l)
	| Descr_Prcd(_,_,_) ->		raise (RESIDENT_EVIL "It's a Procedure!")

let get_residence_vec (i:ide) (off:int) (r:env) :value = match r#get i with
	  Descr_Vctr(_,lb,ub,Loc(l)) ->	let real = l - lb + off in StoreLoc(Loc(real))
	| _ ->							raise (MY_FAULT "get_residence_vec")

(* I need THAT value, I don't mind where is it saved. *)
let get_value (l:value) (s:store) (h:heap) :value = match l with
	  StoreLoc(sl) ->	s#get sl
	| HeapLoc(hl) ->	h#get hl
	| _ -> raise (MY_FAULT "get_value")

let get_name (i:ide) = match i with Ide(name) -> name

(* I need something to get changed: do it and don't bother me *)
let set_value (l:value) (v:value) (s:store) (h:heap) = 
(*	print_string ("\n\t"^(string_of_value l)^"->"^(string_of_value v)); *)
	match l with
	  StoreLoc(sl) ->	s#set sl v
	| HeapLoc(hl) ->	h#set hl v
	| _ -> raise (MY_FAULT "set_value")

(** END OF FUFFA **)

(* evaluation of declarations *)
let rec decl_eval (d:dec list) (r:env) (s: store) (h:heap) = ( match d with
      []                        ->  ()
    | Dec(x,Basic(tipo))::decls ->  let nl = s#newmem
                                    in (
                                         match tipo with
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
    | Dec(x,Pointer(pcontent))::decls ->	let nl = s#newmem
												and depth = pntr_depth pcontent
												and ptype = pntr_finaltype pcontent
											in (
												r#set x (Descr_Pntr(ptype,depth,nl));
												s#set nl (StoreLoc(Null));
												decl_eval decls r s h
											)
    | Dec(x,Vector(t,lb,ub))::decls ->  	let nl = s#newmem and dim = ub - lb + 1 in
						                        let vo = moveloc nl lb in
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
and eval_aexp (e:aexp) (r:env) (s:store) (h:heap): value = ( match e with
      N(n)      ->  ValueInt(n)
    | R(n)      ->  ValueFloat(n)
    | Ident(i)  ->  (match r#get i with
                          Var(_,l) ->			s#get l
                        | Val(v) ->				v
                        | Descr_Pntr(_,_,l) ->	s#get l
                        | _ ->					match i with Ide(name) -> raise (SYNTAX ("Eval_aexp(Ident): Id not found("^name^")") )
                    )
    | Unref(p)  ->	let (idaddr, depth) = (pntr_get_data p r) in	(* First I get the unreference last location *)
    					do_deref (depth+1) idaddr s h				(* Then I get that final location stored value *)
    | Ref(i)    ->	get_residence i r
    | Malloc(t) ->	(match t with
						  Basic(b) ->			let l = h#newmem in
						  							(match b with
						  								  Int -> h#set l (ValueInt(0));
						  								| Float -> h#set l (ValueFloat(0.0));
						  							); HeapLoc(l)
						| Const(_,_) ->			raise (SYNTAX "You don't want to declare dynamic constant, do you?")
						| Pointer(p) ->			let l = h#newmem in
													(match p with 
														_ -> h#set l (HeapLoc(Null))
													); HeapLoc(l)
						| Vector(b,lb,ub) ->	let hm = 1 + ub - lb in
													let fc = h#lnewmem hm in
														let n = mkid fc in
															decl_eval ([Dec(n,Vector(b,lb,ub))]) r s h;
														(match b with
															  Int ->	h#set_vec fc (ValueInt(0)) hm; HeapLoc(fc)
															| Float ->	h#set_vec fc (ValueFloat(0.0)) hm; HeapLoc(fc)
														);
					)
    | Vec(v,i)  ->  (match r#get v with
						  Descr_Vctr(_,lb,ub,Loc(vo)) ->	(let res = (eval_aexp i r s h) in
																match res with 
																	  ValueInt(pos) ->	if (pos >= lb && pos <= ub)
																	  						then s#get (Loc(vo+pos))
																							else raise (INDEX_OUT_OF_BOUNDS "eval_aexp:vec:I" )
																	| _ ->				raise (INDEX_OUT_OF_BOUNDS "eval_aexp:vec:II")
															)
						| _ -> raise (SYNTAX "Eval_aexp(Vec): Not a Descr_Vector")
					)
    
    | Sum (a,b) ->	aexp_op_fun a b r s h (+) (+.)
    | Sub (a,b) ->	aexp_op_fun a b r s h (-) (-.)
    | Mul (a,b) ->	aexp_op_fun a b r s h ( * ) ( *. )
    | Div (a,b) ->	aexp_op_fun a b r s h (/) (/.)

) and aexp_op_fun  (a:aexp) (b:aexp) (r:env) (s:store) (h:heap) fi fr = (
	let aValue = (eval_aexp a r s h) and bValue = (eval_aexp b r s h) in
		(match aValue,bValue with 
			  ValueInt(op1),ValueInt(op2) ->		ValueInt(fi op1 op2)
			| ValueFloat(op1),ValueFloat(op2) ->	ValueFloat(fr op1 op2)
			| ValueInt(_),ValueFloat(_) ->			raise (DIFFERENT_TYPE_OPERATION "aexp_op_fun:I")
			| ValueFloat(_),ValueInt(_) ->			raise (DIFFERENT_TYPE_OPERATION "aexp_op_fun:II")
			| _,_ ->								raise (POINTER_ARITHMETIC "aexp_op_fun")
		)
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
let rec sub_prog_decl_eval (d: sub_prog list) (r:env) (s:store) (h:heap) = match d with
      [] -> ()
    | Proc(x,params,locals,cmds)::decls ->	r#set x (Descr_Prcd(params,locals,cmds));
    										sub_prog_decl_eval decls r s h


(* evaluation of actual parameter list *)
let rec eval_actual_params (e:aexp list) (r:env) (s:store) (h:heap) = match e with
      []        ->  [] 
    | esp::vl   ->  (eval_aexp esp r s h)::(eval_actual_params vl r s h)

(* prepare for execution of subprograms *)
let type_checking (input_values:value list) (parameters:param list) = 
    let rec check_types (actuals:value list) (formals:param list) :bool =
        match (actuals,formals) with
              [],[] -> true
            | (v)::acts,Par(id,tipo)::forms ->
                (
                 match v with
                      ValueInt(_) ->	if (tipo = Int) then (check_types acts forms)
										else false
	                | ValueFloat(_) ->	if (tipo = Float) then (check_types acts forms)
										else false
                    | StoreLoc(_) ->	raise (SYNTAX "You cannot use StoreLoc")
                    | HeapLoc(_) ->		raise (SYNTAX "You cannot use HeapLoc")
                )
            | _ -> raise (SYNTAX "Type_checking: error")
    in
        if (List.length(input_values) != List.length(parameters))
            then false
        else
            (check_types input_values parameters)

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
      Ass(i,e) ->		let ret = eval_aexp e r s h in
							(match i with
								  LVar(id)  ->		let l = get_residence id r in
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
								| LVec(v,idx) ->	(match r#get v with
														  Descr_Vctr(_,lb,ub,Loc(vo)) ->
																(let res = (eval_aexp idx r s h) in
																	(match res with 
																		ValueInt(pos) -> if (pos >= lb && pos <= ub)
																			then s#set (Loc(vo+pos)) ret
																			else raise (INDEX_OUT_OF_BOUNDS "exec:Ass:LVec:I")
																		| _ -> raise (INDEX_OUT_OF_BOUNDS "exec:Ass:LVec:II")
																	)
																)
														| _ -> raise (SYNTAX "Exec(Ass,LVec): Not a Descr_Vector")
													)

								| Lunref(u) ->		(let (idaddr, depth) = pntr_get_data u r in
														set_value (do_deref (depth) idaddr s h) ret s h
													)
                        	)
    | Blk([]) ->		()
    | Blk(x::y) ->		exec x r s h;
    					exec (Blk(y)) r s h
    | Ite(b,c1,c2) ->	if (eval_bexp b r s h) then (exec c1 r s h)
                        else (exec c2 r s h)
    | While(b,c) ->		if (not(eval_bexp b r s h)) then exec (While(b,c)) r s h
    | For(i,valmin_exp,valmax_exp,c) ->
						let valmin = eval_aexp valmin_exp r s h
                        	and update_counter l s = (match (s#get l) with
								  ValueInt(n) ->	s#set l (ValueInt(n + 1))
								| _ ->				raise (SYNTAX "Use an integer for your counter, do NOT use other stuff")
							) in 
							(match r#get i with
								  Var(_,l) ->	(
												s#set l valmin;
												let rec exec_for s =
													exec c r s h;
													let ret = (eval_bexp (LT(Ident(i), valmax_exp)) r s h) in
														if (ret) then (update_counter l s; exec_for s)
												in exec_for s
												)
								| _ ->			raise (SYNTAX "Exec(For): Not a Variable")
							)
    | Repeat(c,b)   ->  exec c r s h;
						if (not(eval_bexp b r s h)) then exec (Repeat(c,b)) r s h
    | Write(e)      ->  let ret = (eval_aexp e r s h) in
						(match ret with
							  ValueInt(op1) ->		print_string ("\nInt:\t"^(string_of_int op1))
							| ValueFloat(op1) ->	print_string ("\nFlt:\t"^(string_of_float op1) )
							| StoreLoc(op1) ->		print_string ("\nSLc:\t"^(string_of_loc op1))
							| HeapLoc(op1) ->		print_string ("\nHLc:\t"^(string_of_loc op1))
						)
    | PCall(id,input_exprs)
                    ->  let input_values = (eval_actual_params input_exprs r s h) in
						(match (r#get id) with
							  Descr_Prcd(params,locals,cmds) -> if ((type_checking input_values params))
							  										then exec_proc id input_values r s h
																	else raise (PARAMETERS_DO_NOT_MATCH "Exec:PCall")
							| _ -> raise (SYNTAX "Exec(Pcall): Not a Descr_Procedure")
						)
	| Free(p) ->		(let lookforloc (p:lexp) = (
							match p with
								  LVar(id) ->		get_value (get_residence id r) s h
								| LVec(id,aexp) ->	(let off = (eval_aexp aexp r s h) in
														match off with
															  ValueInt(v) ->	get_value (get_residence_vec id v r) s h
															| _ ->				raise (INDEX_OUT_OF_BOUNDS "Exec:Free")
													)
								| Lunref(d) ->		raise (NOT_YET_IMPLEMENTED "")
							) in let d = lookforloc p in match d with
								  HeapLoc(l) ->	h#free l
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

