(* (2003)Originale ML(Marco Pistore & Floriano Zini) *)
(*    (2004)Estensione OCAML(Fabrizio Lazzarotto)    *)
(*          (2007)Revisione (Stefano Schivo)         *)

open Syntaxtree;;
open Heap;;

(******************************)

(* THE INTERPRETER *)

(* utility functions *)
let initenv (x:ide):env_entry = raise NO_IDE
let initmem (x:loc):value = raise NO_MEM
let initheap = new heap 16 (* Initial empty Heap *)

let updatemem ((s:store), addr, (v:value)) :store = function
    x -> if (x = addr) then v else s(x)

let updateenv ((e:env),id, (v:env_entry)) :env = function
    y -> if (y = id) then v else e(y)

let newmem (s: store) : int =   
    let rec aux n =
        try (let _=s(Loc(n)) in aux(n+1)) with NO_MEM -> n (* s(Loc(n)) la usiamo solo per leggere la memoria alla locazione n, così da vedere se quella locazione è non-valida (cioè libera), e quindi non ne consideriamo il valore *)
    in aux 0

let rec updatemem_vector((s:store), addr, length, (v:value)) :store =
    match length with
          1 ->  updatemem(s,addr,v)
        | n ->  let news = updatemem(s,addr,v)
                in
                    updatemem_vector(news,Loc(newmem news),n-1,v)

(** START OF FUFFA **)

(* Calculate pointer depth *)
let pntr_depth (p:pType) : int =
	let rec aux p = match p with
		  SPointer(_) -> 0
		| MPointer(next) -> 1 + (aux next)
	in aux p
	(*
	let res = aux p
	  	in print_string ("\nLOL puntatore di " ^ (string_of_int res) ^ " gradi\n"); res
	*)

let pntr_finaltype (p:pType) : bType =
	let rec aux p = match p with
		SPointer(v) -> v
		| MPointer(next) -> aux next
	in aux p

(* Get final identifier name from a pointer *)
let pntr_get_data (p:dexp) (r:env) : (loc * int) =
	let rec aux (p:dexp) (r:env) = match p with
		  Sunref(id) -> (
		  				match r(id) with
		  					  Var(l) -> (l, 1)
		  					| Descr_Pntr(n,l) -> (l,1)
		  					| _ -> raise LOL_DUNNO
		  				)
		| Munref(next) -> let (a,b) = aux next r in (a, b+1)
	in aux p r

let rec do_deref (depth:int) (l:loc) (s:store) : value = 
	let res = s(l) in
		if depth == 0 then 
			res
		else
			match res with
				ValueLoc(newl) -> do_deref (depth - 1) newl s
				| _ -> raise (SYNTAX "Do_deref: Not a ValueLoc")

let get_addr (d:lexp) (r:env) (s:store) : loc = match d with
	  LVar(id) -> (
	  				match r(id) with
	  					  Descr_Pntr(_,l) -> l
	  					| _ -> raise (SYNTAX "I don't like what you're trying to do...")
	  			)
	| LVec(v,off) -> raise (SYNTAX "Oh no you don't want to do that|")
	| Lunref(p) ->	let (idaddr, depth) = pntr_get_data p r 
	  					in let res = do_deref depth idaddr s
							in (
								match res with
									ValueLoc(l) -> l
								)
	| _ -> raise (SYNTAX "WTF are you doing?")

(** END OF FUFFA **)

(* evaluation of arithmetical expressions *)
let rec eval_aexp (e:aexp) (r:env) (s:store) (h:heap): value = match e with
      N(n)      ->  ValueInt(n)
    | R(n)      ->  ValueFloat(n)
    | Ident(i)  ->  (
                     match r(i) with
                          Var(l) -> s(l)
                        | Val(v) -> v
                        | Descr_Pntr(n,l) -> s(l)
                        | _ -> match i with Ide(name) -> raise (SYNTAX ("Eval_aexp(Ident): Id not found("^name^")") )
                    )
    | Deref(p)  -> (
    				
    				let (idaddr, depth) = pntr_get_data p r in
    					let res = do_deref depth idaddr s
    						in res
    			   )
    | Ref(p)    -> (
    	
					let rec aux (p:rexp) : value = match p with
						  Sref(i) -> (
						  				match r(i) with 
						  					  Var(l) -> ValueLoc(l) (* Return variable address *)
							  				| Descr_Pntr(_,l) -> ValueLoc(l) (* Return pointer address *)
							  				| Descr_Vector(l,_,_) -> ValueLoc(l) (* Return vector address *)
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
					match t with
						  Basic(b) ->			let l = h#newmem 1 in
							  						(match b with
							  							  Int -> h#update l (ValueInt(0))
							  							| Float -> h#update l (ValueFloat(0.0))
							  						); ValueLoc(l)
						| Const(_,_) ->			raise (SYNTAX "You don't want to declare dynamic constant, do you?")
						| Pointer(p) -> 		let l = h#newmem 1 in
													(match p with 
														_ -> h#update l (ValueLoc(Loc(0)))
													); ValueLoc(l)
						| Vector(v,lb,ub) ->	let s = ub - lb in
													let l = h#newmem s in
														(match v with
															  Int -> h#updatevec l s (ValueInt(0))
															| Float -> h#updatevec l s (ValueFloat(0.0))
														); ValueLoc(l)
    				)
    | Vec(v,i)  ->  (
                     match r(v) with
                          Descr_Vector(Loc(vo),lb,ub) ->
							(
								let ValueInt(pos) = (eval_aexp i r s h)
		                        in
		                            if (pos >= lb && pos <= ub) then
		                                s(Loc(vo+pos))
		                            else
		                                raise INDEX_OUT_OF_BOUNDS
							)
                        | _ -> raise (SYNTAX "Eval_aexp(Vec): Not a Descr_Vector")
                    )
    
    | Sum (a,b) ->  aexp_op_fun a b r s h (+) (+.)
    
    | Sub (a,b) ->  aexp_op_fun a b r s h (-) (-.)
    
    | Mul (a,b) ->  aexp_op_fun a b r s h ( * ) ( *. )
    				(*
    				let mi a b = a*b
                    in 
                        let mf a b = a*.b
                        in aexp_op_fun a b r s mi mf
    				*)
    
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
			| ValueLoc(Loc(l)) -> raise (SYNTAX ("Location (" ^ (string_of_int l) ^ ")"))
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
    | Dec(x,Basic(tipo))::decls ->  let newaddr = newmem s
                                    in (
                                         match tipo with
                                              Int   -> dec_eval decls (updateenv(r,x,Var(Loc(newaddr)))) (updatemem(s,Loc(newaddr),ValueInt(0))) h
                                            | Float -> dec_eval decls (updateenv(r,x,Var(Loc(newaddr)))) (updatemem(s,Loc(newaddr),ValueFloat(0.0))) h
                                        )
    | Dec(x,Const(tipo,valore_exp))::decls  
                                ->  let valore = eval_aexp valore_exp r s h
                                    in 
                                    (
                                      match tipo with
                                           Int  -> 
                                            (
                                             match valore with
                                                  ValueInt(v) -> dec_eval decls (updateenv(r,x,Val(ValueInt(v)))) s h
                                                | ValueFloat(v) -> raise DIFFERENT_TYPE_ASSIGNATION
                                                | ValueLoc(Loc(l)) -> raise DIFFERENT_TYPE_ASSIGNATION
                                            )
                                         | Float->
                                            (
                                             match valore with
                                                  ValueFloat(v) -> dec_eval decls (updateenv(r,x,Val(ValueFloat(v)))) s h
                                                | ValueInt(v) -> raise DIFFERENT_TYPE_ASSIGNATION
                                                | ValueLoc(Loc(l)) -> raise DIFFERENT_TYPE_ASSIGNATION
                                            )
                                    )
    | Dec(x,Pointer(pcontent))::decls ->
    		let newaddr = newmem s
    			and depth = pntr_depth pcontent
    		in (
    			(* print_string ("New Descr_Pntr(" ^ (string_of_int depth) ^ ", " ^ (string_of_int newaddr) ^ ")"); *)
    			dec_eval decls (updateenv(r,x,Descr_Pntr(depth,Loc(newaddr)))) (updatemem(s,Loc(newaddr),ValueLoc(Loc(0)))) h
    		)
    | Dec(x,Vector(tipo,lb,ub))::decls
                                ->  let newaddr = newmem s
                                    and dim = ub - lb + 1
                                    in
                                    let vo = Loc(newaddr - lb)
                                    in
                                    (
                                      match tipo with
                                          Int -> dec_eval decls (updateenv(r,x,Descr_Vector(vo,lb,ub))) (updatemem_vector(s,Loc(newaddr),dim,ValueInt(0))) h
                                        | Float -> dec_eval decls (updateenv(r,x,Descr_Vector(vo,lb,ub))) (updatemem_vector(s,Loc(newaddr),dim,ValueFloat(0.0))) h
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
                    | ValueLoc(Loc(l)) ->
                    	raise (SYNTAX "You cannot use ValueLoc")
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
                      Var(l) -> (r',updatemem(s',l,v),h)
                    | _ -> raise (SYNTAX "Assign_values: Not a Variable")
                )
        | _ -> raise (SYNTAX "Assign_values: Not a list")





(* execution of commands *)
let rec exec (c: cmd) (r: env) (s: store) (h:heap) = match c with
    Ass(i,e)        ->  let ret = eval_aexp e r s h
                        in 
                        (
                         match i with
                              LVar(id)  -> (
                                            match r(id) with
                                              Var(l)    -> updatemem(s,l,ret)
                                            | Descr_Pntr(n,l) -> updatemem(s,l,ret)
                                            | _         -> (
		                                        			match id with 
		                                        				Ide(name) -> raise (SYNTAX ("Exec(Ass,LVar): Not a Variable("^name^")"))
                                            				)
                                           )
                            | LVec(v,idx) -> (
                                              match r(v) with
                                                  Descr_Vector(Loc(vo),lb,ub) ->
                                                    let ValueInt(pos) = (eval_aexp idx r s h)
                                                    in
                                                        if (pos >= lb && pos <= ub) then
                                                            updatemem(s,Loc(vo+pos),ret)
                                                        else
                                                            (
                                                             raise INDEX_OUT_OF_BOUNDS
                                                            )
                                                | _ -> raise (SYNTAX "Exec(Ass,LVec): Not a Descr_Vector")
                                             )
                        	| Lunref(u) -> 	( let (idaddr, depth) = pntr_get_data u r in
												let res = do_deref depth idaddr s
													in match res with 
														  ValueLoc(l) -> updatemem(s,l,ret)
														| ValueInt(l) -> updatemem(s,Loc(l),ret)
														| _ -> raise LOL_DUNNO
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
                            match s(l) with
                              ValueInt(n) -> updatemem(s, l, ValueInt(n + 1))
                            | ValueFloat(f) -> updatemem(s, l, ValueFloat(f +. 1.0))
                            | ValueLoc(_) -> raise LOL_DUNNO
                        in 
                        (
                         match r(i) with
                              Var(l) -> 
                                (
                                let s0 = updatemem(s, l, valmin)
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
                              ValueInt(op1) -> print_string("Int:\t"); print_int(op1); print_string "\n";s
                            | ValueFloat(op1) -> print_string("Float:\t"); print_float(op1); print_string "\n";s
                            | ValueLoc(Loc(op1)) -> print_string("Loc:\t"); print_int(op1); print_string "\n";s
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
	| Free(p) ->		let l = get_addr p r s
							in h#delete l; s


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
        Program(vars,sub_progs,com) ->   let (r,s,h) = sub_prog_decl_eval sub_progs (dec_eval vars initenv initmem initheap)
                                         in (exec com r s h)
      | Null -> initmem
