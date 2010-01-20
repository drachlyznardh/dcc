(* (2003)Originale ML(Marco Pistore & Floriano Zini) *)
(*    (2004)Estensione OCAML(Fabrizio Lazzarotto)    *)
(*          (2007)Revisione (Stefano Schivo)         *)

open Syntaxtree;;
open Mem;;

(******************************)

(* THE INTERPRETER *)

(* utility functions *)
let initenv (x:ide):env_entry = raise NO_IDE
let initmem (x:loc):value = raise NO_MEM

let initstore = new store 16	(* Initially empty Store *)
let initheap = new heap 16		(* Initially empty Heap *)

(*
let updatemem ((s:store), (l:loc), (v:value)) :store = 
	print_string ("["^(string_of_loc l)^":"^(string_of_value v)^"]\n");
	function
    x -> if (x = l) then v else s(x)
*)

let updateenv ((e:env),id, (v:env_entry)) :env = function
    y -> if (y = id) then v else e(y)

(*
let newmem (s: store) : int =   
    let rec aux n =
        try (let _=s(Loc(n)) in aux(n+1)) with NO_MEM -> n (* s(Loc(n)) la usiamo solo per leggere la memoria alla locazione n, cos� da vedere se quella locazione � non-valida (cio� libera), e quindi non ne consideriamo il valore *)
    in aux 0
*)

(*
let rec updatemem_vector((s:store), addr, length, (v:value)) :store =
    match length with
          1 ->  s#update addr v; s
(*          1 ->  s#update addr v; s *)
(*        | n ->  let news = updatemem(s,addr,v) *)
        | n ->  let news = s#update addr v
                in
                    updatemem_vector(news,Loc(s#newmem news),n-1,v)
*)

(** START OF FUFFA **)

(* Calculate pointer depth *)
let pntr_depth (p:pType) : int =
	let rec aux p = match p with
		  SPointer(_) -> 0
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
		  Sunref(id) -> (
		  				match r(id) with
		  					  Var(l) -> (StoreLoc(l), 1)
		  					| Val(v) -> (v,1)
		  					| Descr_Pntr(n,l) -> (StoreLoc(l),1)
		  					| _ -> raise NOT_A_POINTER
		  				)
		| Munref(next) -> let (a,b) = aux next r in (a, b+1)
	in aux p r
	
let rec do_deref_value (depth:int) (v:value) (s:store) (h:heap) : value = 
	if depth = 0 then
		v
	else match v with
		  StoreLoc(l) -> do_deref_value (depth - 1) (s#get l) s h
		| HeapLoc(l) -> do_deref_value (depth - 1) (h#get_value l) s h
		| ValueInt(v) -> raise (SYNTAX ("Do_deref_value: ValueInt("^(string_of_int v)^")is not a ValueLoc"))
		| ValueFloat(v) -> raise (SYNTAX ("Do_deref_value: ValueFloat("^(string_of_float v)^") is not a ValueLoc"))

let get_addr (d:lexp) (r:env) (s:store) (h:heap) : loc = match d with
	  LVar(id) -> (
	  				match r(id) with
	  					  Descr_Pntr(_,l) ->	(match (s#get l) with
	  					  							  HeapLoc(v) -> print_string ("HeapLoc["^(string_of_loc v)^"]"); v
	  					  							| StoreLoc(v) -> print_string ("StoreLoc["^(string_of_loc v)^"]"); v
	  					  							| _ -> raise (DEREF_ON_NOT_A_POINTER ("Get_addr["^(string_of_loc l)^"]"))
	  					  						)
	  					| _ -> raise (SYNTAX "I don't like what you're trying to do...")
	  			)
	| LVec(v,off) -> raise (SYNTAX "Oh no you don't want to do that|")
	| Lunref(p) ->	let (idaddr, depth) = pntr_get_data p r 
	  					in let res = do_deref_value depth idaddr s h
							in get_loc res

(** END OF FUFFA **)

(* evaluation of arithmetical expressions *)
let rec eval_aexp (e:aexp) (r:env) (s:store) (h:heap): value = match e with
      N(n)      ->  ValueInt(n)
    | R(n)      ->  ValueFloat(n)
    | Ident(i)  ->  (
                     match r(i) with
                          Var(l) -> s#get l
                        | Val(v) -> v
                        | Descr_Pntr(n,l) -> s#get l
                        | _ -> match i with Ide(name) -> raise (SYNTAX ("Eval_aexp(Ident): Id not found("^name^")") )
                    )
    | Deref(p)  -> (
    				
    				let (idaddr, depth) = (pntr_get_data p r) in
    					let res = (do_deref_value depth idaddr s h)
    						in res
    			   )
    | Ref(p)    -> (
    	
					let rec aux (p:rexp) : value = match p with
						  Sref(i) ->	(
						  				match r(i) with 
						  					  Var(l) -> StoreLoc(l) (* Return variable address *)
							  				| Descr_Pntr(_,l) ->	(match s#get l with
							  											  StoreLoc(v) -> s#get v
							  											| HeapLoc(v) -> h#get_value v
						  												| _ -> raise NOT_A_POINTER
							  										)
							  				(* Return pointer address *)
							  				| Descr_Vector(l,_,_) -> StoreLoc(l) (* Return vector address *)
							  				| _ ->
							  					(
							  						match i with
							  							Ide(name) -> raise (SYNTAX ("Eval_aexp(Ref): Not a Descr_Pntr ("^name^")"))
							  					)
						  				)
						| Mref(next) -> aux next
					in aux p
    			   )
    | Malloc(t) -> (
    				h#show;
					(match t with
						  Basic(b) ->	let l = h#newmem 1 in
					  						(match b with
					  							  Int -> h#set_value l (ValueInt(0));
					  							| Float -> h#set_value l (ValueFloat(0.0));
					  						); HeapLoc(l)
						| Const(_,_) ->	raise (SYNTAX "You don't want to declare dynamic constant, do you?")
						| Pointer(p) -> let l = h#newmem 1 in
											(match p with 
												_ -> h#set_value l (HeapLoc(Null))
											); print_string ("Malloc(Pointer)"^(string_of_value (HeapLoc(l)))^"\n"); h#show; HeapLoc(l)
						| Vector(_,_,_) ->	raise (SYNTAX "Just no.")
					)
    				)
    | Vec(v,i)  ->  (
                     match r(v) with
                          Descr_Vector(Loc(vo),lb,ub) ->
							(
								let res = (eval_aexp i r s h) in match res with 
									  ValueInt(pos) ->	if (pos >= lb && pos <= ub) then
		                                					s#get (Loc(vo+pos))
												        else
												            raise INDEX_OUT_OF_BOUNDS
		                        	| _ -> raise INDEX_OUT_OF_BOUNDS
							)
                        | _ -> raise (SYNTAX "Eval_aexp(Vec): Not a Descr_Vector")
                    )
    
    | Sum (a,b) ->  aexp_op_fun a b r s h (+) (+.)
    
    | Sub (a,b) ->  aexp_op_fun a b r s h (-) (-.)
    
    | Mul (a,b) ->  aexp_op_fun a b r s h ( * ) ( *. )
    
    | Div (a,b) -> aexp_op_fun a b r s h (/) (/.)

and aexp_op_fun  (a:aexp) (b:aexp) (r:env) (s:store) (h:heap) fi fr = 
    let aValue = (eval_aexp a r s h)
        and bValue = (eval_aexp b r s h)
    in (
         match aValue with 
             ValueInt(op1)      -> (
                                     match bValue with
                                          ValueInt(op2) -> ValueInt(fi op1 op2)
                                        | _ -> raise DIFFERENT_TYPE_OPERATION
                                    )
            | ValueFloat(op1)   -> (
                                     match bValue with
                                          ValueFloat(op2) -> ValueFloat(fr op1 op2)
                                        | _ -> raise DIFFERENT_TYPE_OPERATION
                                    )
			| StoreLoc(l) -> raise (SYNTAX ("Location (" ^ (string_of_loc l) ^ ")"))
			| HeapLoc(l) -> raise (SYNTAX ("Location (" ^ (string_of_loc l) ^ ")"))
        )

let rec eval_bexp (e:bexp) (r:env) (s:store) (h:heap) = match e with
      B(b)      ->  b
    | And (a,b) ->  ((eval_bexp a r s h) && (eval_bexp b r s h))
    | Or  (a,b) ->  ((eval_bexp a r s h) || (eval_bexp b r s h))
    | Equ (a,b) ->  ((eval_aexp a r s h)  = (eval_aexp b r s h))
    | LE  (a,b) ->  ((eval_aexp a r s h) <= (eval_aexp b r s h))
    | LT  (a,b) ->  ((eval_aexp a r s h)  < (eval_aexp b r s h))
    | Not (a)   ->  (not(eval_bexp a r s h))


(* evaluation of declarations *)
let rec dec_eval (d:dec list) (r:env) (s: store) (h:heap) = match d with
      []                        ->  (r,s,h)
    | Dec(x,Basic(tipo))::decls ->  let nl = s#newmem
                                    in (
                                         match tipo with
                                              Int   ->	s#update nl (ValueInt(0));
                                              			dec_eval decls (updateenv(r,x,Var(nl))) s h
                                            | Float ->	s#update nl (ValueFloat(0.0));
                                            			dec_eval decls (updateenv(r,x,Var(nl))) s h
                                        )
    | Dec(x,Const(tipo,valore_exp))::decls  
                                ->  let valore = eval_aexp valore_exp r s h in
                                		(match (tipo,valore) with
                                			  (Int,ValueInt(v)) -> dec_eval decls (updateenv(r,x,Val(ValueInt(v)))) s h
                                			| (Float,ValueFloat(v)) -> dec_eval decls (updateenv(r,x,Val(ValueFloat(v)))) s h
                                			| (_,_) -> raise DIFFERENT_TYPE_ASSIGNATION
                                		)
    | Dec(x,Pointer(pcontent))::decls ->
    		let nl = s#newmem
    			and depth = pntr_depth pcontent
    		in (
    			(* print_string ("New Descr_Pntr(" ^ (string_of_int depth) ^ ", " ^ (string_of_int newaddr) ^ ")"); *)
    			s#update nl (StoreLoc(Null));
    			dec_eval decls (updateenv(r,x,Descr_Pntr(depth,nl))) s h
    		)
    | Dec(x,Vector(tipo,lb,ub))::decls
                                ->  let nl = s#newmem and dim = ub - lb + 1 in
	                                    let vo = moveloc nl lb in
				                            (match tipo with
				                                  Int ->	s#updatevec (nl) dim (ValueInt(0));
				                                  			dec_eval decls (updateenv(r,x,Descr_Vector(vo,lb,ub))) s h
				                                | Float ->	s#updatevec (nl) dim (ValueFloat(0.0));
				                                			dec_eval decls (updateenv(r,x,Descr_Vector(vo,lb,ub))) s h
				                            )

(* declaration of subprograms *)
let rec sub_prog_decl_eval (d: sub_prog list) ((r:env),(s:store),(h:heap)) = match d with
      [] -> (r,s,h)
    | Proc(id,params,locals,cmds)::decls ->
        sub_prog_decl_eval decls ((updateenv(r,id,(Descr_Procedure(params,locals,cmds)))),s,h)


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
                      ValueInt(x) ->
                        if (tipo=Int) then
                            (check_types acts forms)
                        else false
                    | ValueFloat(x) ->
                        if (tipo=Float) then
                            (check_types acts forms)
                        else false
                    | StoreLoc(_) -> raise (SYNTAX "You cannot use StoreLoc")
                    | HeapLoc(_) -> raise (SYNTAX "You cannot use HeapLoc")
                )
            | _ -> raise (SYNTAX "Type_checking: error")
    in
        if (List.length(input_values)!= List.length(parameters))
            then false
        else
            (check_types input_values parameters)


let rec assign_values (formals:param list) (actuals:value list) ((r:env), (s:store), (h:heap)) =
    match (formals,actuals) with
          [],[] -> (r,s,h)
        | Par(id,tipo)::forms,v::acts ->
            let (r',s',h') =
                assign_values forms acts (dec_eval [Dec(id,Basic(tipo))] r s h)
            in
                (
                 match r'(id) with
                      Var(l) -> s#update l v; (r',s,h)
                    | _ -> raise (SYNTAX "Assign_values: Not a Variable")
                )
        | _ -> raise (SYNTAX "Assign_values: Not a list")


let move_pointer (l:value) (v:value) (s:store) (h:heap) : store = match l with
	  HeapLoc(hl) ->	let nl = (get_loc v) in h#sage hl; h#show; h#bump nl; h#show; s#update hl v; s
	| StoreLoc(sl) ->	let nl = (get_loc v) in h#bump nl; h#show; s#update sl v; s
	| _ ->				raise NOT_A_POINTER

(* execution of commands *)
let rec exec (c: cmd) (r: env) (s: store) (h:heap) = match c with
    Ass(i,e)        ->  let ret = eval_aexp e r s h
                        in
                        (
                         match i with
                              LVar(id)  -> (
                                            match r(id) with
                                              Var(l)    -> s#update l ret; s
                                            | Descr_Pntr(_,l) ->	move_pointer (s#get l) ret s h
                                            | _         -> (
		                                        			match id with 
		                                        				Ide(name) -> raise (SYNTAX ("Exec(Ass,LVar): Not a Variable("^name^")"))
                                            				)
                                           )
                            | LVec(v,idx) -> (
                                              match r(v) with
                                                  Descr_Vector(Loc(vo),lb,ub) ->
														(
														let res = (eval_aexp idx r s h) in
		                                                	(match res with 
		                                                		  ValueInt(pos) ->
						                                                if (pos >= lb && pos <= ub)
						                                                	then (s#update (Loc(vo+pos)) ret; s)
								                                        else 
								                                            raise INDEX_OUT_OF_BOUNDS
		                                                       	| _ -> raise INDEX_OUT_OF_BOUNDS
                                                			)
                                                		)
                                                | _ -> raise (SYNTAX "Exec(Ass,LVec): Not a Descr_Vector")
                                             )
                        	| Lunref(u) -> 	( let (idaddr, depth) = pntr_get_data u r in
												let res = do_deref_value depth idaddr s h
													in match res with 
														  StoreLoc(l) -> s#update l ret; s
														| HeapLoc(l) -> move_pointer (h#get_value l) ret s h
														| ValueInt(v) -> raise (DEREF_ON_NOT_A_POINTER ("Lunref("^(string_of_int v)^")"))
														| ValueFloat(v) -> raise (DEREF_ON_NOT_A_POINTER ("Lunref("^(string_of_float v)^")"))
                        					)
                        )
    | Blk([])       ->  s
    | Blk(x::y)     ->  exec (Blk(y)) r (exec x r s h) h
    | Ite(b,c1,c2)  ->  if (eval_bexp b r s h) then (exec c1 r s h)
                        else (exec c2 r s h)
    | While(b,c)    ->  if (not(eval_bexp b r s h)) then s
                        else
                            let s'' = exec c r s h
                            in (exec (While(b,c)) r s'' h)
    | For(i,valmin_exp,valmax_exp,c) 
                    ->  let valmin = eval_aexp valmin_exp r s h
                        and update_counter l s =
                            match (s#get l) with
                              ValueInt(n) -> s#update l (ValueInt(n + 1)); s
                            | _ -> raise (SYNTAX "Use an integer for your counter, do NOT use other stuff")
                        in 
                        (
                         match r(i) with
                              Var(l) -> 
                                (
                                s#update l valmin;
                                let s0 = s
                                in
                                    let rec exec_for s =
                                        let s' = exec c r s h
                                        in
                                            let ret = eval_bexp (LT(Ident(i), valmax_exp)) r s' h
                                            in
                                                if (ret) then exec_for (update_counter l s')
                                                else s'
                                    in exec_for s0
                                )
                            | _ -> raise (SYNTAX "Exec(For): Not a Variable")
                        )
    | Repeat(c,b)   ->  let s' = exec c r s h
                        in
                            let ret = eval_bexp b r s' h
                            in
                                if (ret) then s'
                                else (exec (Repeat(c,b)) r s' h)
    | Write(e)      ->  let ret = (eval_aexp e r s h)
                        in
                        (
                         match ret with
                              ValueInt(op1) ->		print_string ("Int:\t"^(string_of_int op1)^"\n");	s
                            | ValueFloat(op1) ->	print_string ("Flt:\t"^(string_of_float op1)^"\n"); s
                            | StoreLoc(op1) ->		print_string ("SLc:\t"^(string_of_loc op1)^"\n");	s
                            | HeapLoc(op1) ->		print_string ("HLc:\t"^(string_of_loc op1)^"\n");	s
                        )
    | PCall(id,input_exprs)
                    ->  let input_values = (eval_actual_params input_exprs r s h)
                        in
                        (
                         match (r(id)) with
                              Descr_Procedure(params,locals,cmds) ->
                                if ((type_checking input_values params)) then
                                    exec_proc (r(id)) input_values r s h
                                else
                                    raise PARAMETERS_DO_NOT_MATCH
                            | _ -> raise (SYNTAX "Exec(Pcall): Not a Descr_Procedure")
                        )
	| Free(p) ->		let l = (get_addr p r s h)
							in (match l with 
								Loc(v) -> print_string("Free("^(string_of_int v)^")\n"); h#sage l; s
								| Null -> raise (NULL_POINTER_EXCEPTION "Free")
							)


(* execution of subprograms *)
and exec_proc (ee:env_entry) (input_values:value list) (r: env) (s: store) (h:heap) : store =
    let do_exec inVars locVars cmds (values:value list) (r: env) (s: store) (h:heap) : store =
        let (r',s',h') = assign_values inVars values (r,s,h)
        in
            let (r'',s'',h'') = dec_eval locVars r' s' h'
            in
                exec cmds r'' s'' h''
    in
        match ee with
              Descr_Procedure(params,locals,cmds) ->
                do_exec params locals cmds input_values r s h
            | _ -> raise (SYNTAX "Exec_proc: Not a Descr_Procedure")


(* evaluation of programs *)
let run prog = 
    match prog with
        Program(vars,sub_progs,com) ->   let (r,s,h) = sub_prog_decl_eval sub_progs (dec_eval vars initenv initstore initheap)
                                         in (exec com r s h)
      | NoProg -> initstore
