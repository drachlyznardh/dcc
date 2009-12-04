(* (2003)Originale ML(Marco Pistore & Floriano Zini) *)
(*    (2004)Estensione OCAML(Fabrizio Lazzarotto)    *)
(*          (2007)Revisione (Stefano Schivo)         *)

open Syntaxtree;;
(*** INTERPRETER DOMAINS ***)

(* memory *)
type loc =
	Loc of int						(* Location, position *)

type value =
      ValueInt of int				(* Type integer with value *)
    | ValueFloat of float			(* Type float with value *)

type store =
	loc -> value					(* Store: location to simple values *)

type env_entry =
	  Var of loc					(* Location, variable *)
	| Val of value					(* Value, constant *)
	| Descr_Pntr of loc				(* Pointer to heap location *)
	| Descr_Vector of
		loc * int * int				(* Vector with start point, lower and upper bounds *)
	| Descr_Procedure of
		param list * dec list * cmd	(* Procedure descriptor *)

type env =
	ide -> env_entry				(* Environment entries *)

type heap_entry =
	  Heap_Node of int * int		(* Depth level and pointed location *)
	| Heap_Entry of value			(* End of chain, simple value *)

type heap = loc -> heap_entry		(* Heap entries *)

(* exception *)
exception NO_MEM
exception NO_IDE
exception SYNTAX
exception INDEX_OUT_OF_BOUNDS
exception DIFFERENT_TYPE_OPERATION
exception DIFFERENT_TYPE_ASSIGNATION
exception PARAMETERS_DO_NOT_MATCH

exception NO_HEAP					(* Heap non existent *)
exception NO_SUCH_HEAP_ENTRY		(* Heap entry not found *)

(******************************)

(* THE INTERPRETER *)

(* utility functions *)
let initenv (x:ide):env_entry = raise NO_IDE
let initmem (x:loc):value = raise NO_MEM
let initheap (x:loc):heap_entry = raise NO_HEAP		(* Initial empty Heap *)

let updatemem ((s:store), addr, (v:value)) :store = function
    x -> if (x = addr) then v else s(x)

let updateenv ((e:env),id, (v:env_entry)) :env = function
    y -> if (y = id) then v else e(y)

(*
let newheap (h: heap) : int =
	let rec aux n =
		try (
			let _ = h(Loc(n))
			in aux (n+1)
		) with NO_HEAP -> n
	in aux 0
*)

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


(* evaluation of arithmetical expressions *)
let rec eval_aexp (e:aexp) (r:env) (s:store) : value = match e with
      N(n)      ->  ValueInt(n)
    | R(n)      ->  ValueFloat(n)
    | Ident(i)  ->  (
                     match r(i) with
                          Var(l) -> s(l)
                        | Val(v) -> v
                        | _ -> raise SYNTAX
                    )
    | Vec(v,i)  ->  (
                     match r(v) with
                          Descr_Vector(Loc(vo),lb,ub) ->
                            let ValueInt(pos) = (eval_aexp i r s)
                            in
                                if (pos >= lb && pos <= ub) then
                                    s(Loc(vo+pos))
                                else
                                    raise INDEX_OUT_OF_BOUNDS
                        | _ -> raise SYNTAX
                    )
    
    | Sum (a,b) ->  aexp_op_fun a b r s (+) (+.)
    
    | Sub (a,b) ->  aexp_op_fun a b r s (-) (-.)
    
    | Mul (a,b) ->  let mi a b = a*b
                    in 
                        let mf a b = a*.b
                        in aexp_op_fun a b r s mi mf
    
    | Div (a,b) -> aexp_op_fun a b r s (/) (/.)


and aexp_op_fun  (a:aexp) (b:aexp) (r:env) (s:store) fi fr = 
    let aValue = (eval_aexp a r s)
        and bValue = (eval_aexp b r s)
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
        )


let rec eval_bexp (e:bexp) (r:env) (s:store) = match e with
      B(b)      ->  b
    | And (a,b) ->  ((eval_bexp a r s ) && (eval_bexp b r s ))
    | Or  (a,b) ->  ((eval_bexp a r s ) || (eval_bexp b r s ))
    | Equ (a,b) ->  ((eval_aexp a r s )  = (eval_aexp b r s ))
    | LE  (a,b) ->  ((eval_aexp a r s ) <= (eval_aexp b r s ))
    | LT  (a,b) ->  ((eval_aexp a r s )  < (eval_aexp b r s ))
    | Not (a)   ->  (not(eval_bexp a r s ))


(* evaluation of declarations *)
let rec dec_eval (d:dec list) (r:env) (s: store) = match d with
      []                        ->  (r,s)
    | Dec(x,Basic(tipo))::decls ->  let newaddr = newmem s
                                    in (
                                         match tipo with
                                              Int   -> dec_eval decls (updateenv(r,x,Var(Loc(newaddr)))) (updatemem(s,Loc(newaddr),ValueInt(0)))
                                            | Float -> dec_eval decls (updateenv(r,x,Var(Loc(newaddr)))) (updatemem(s,Loc(newaddr),ValueFloat(0.0)))
                                        )
    | Dec(x,Const(tipo,valore_exp))::decls  
                                ->  let valore = eval_aexp valore_exp r s
                                    in 
                                    (
                                      match tipo with
                                           Int  -> 
                                            (
                                             match valore with
                                                  ValueInt(v) -> dec_eval decls (updateenv(r,x,Val(ValueInt(v)))) s
                                                | ValueFloat(v) -> raise DIFFERENT_TYPE_ASSIGNATION
                                            )
                                         | Float->
                                            (
                                             match valore with
                                                  ValueFloat(v) -> dec_eval decls (updateenv(r,x,Val(ValueFloat(v)))) s
                                                | ValueInt(v) -> raise DIFFERENT_TYPE_ASSIGNATION
                                            )
                                    )
    | Dec(x,Pointer(pcontent))::decls ->
    		let newaddr = newmem s
    			and depth = 0
    		in dec_eval decls (updateenv(r,x,Descr_Pntr(depth))) (updatemem(s,Loc(newaddr),ValueInt(0)))
    | Dec(x,Vector(tipo,lb,ub))::decls
                                ->  let newaddr = newmem s
                                    and dim = ub - lb + 1
                                    in
                                    let vo = Loc(newaddr - lb)
                                    in
                                    (
                                      match tipo with
                                          Int -> dec_eval decls (updateenv(r,x,Descr_Vector(vo,lb,ub))) (updatemem_vector(s,Loc(newaddr),dim,ValueInt(0)))
                                        | Float -> dec_eval decls (updateenv(r,x,Descr_Vector(vo,lb,ub))) (updatemem_vector(s,Loc(newaddr),dim,ValueFloat(0.0)))
                                    )



(* declaration of subprograms *)
let rec sub_prog_decl_eval (d: sub_prog list) ((r:env),(s:store)) = match d with
      [] -> (r,s)
    | Proc(id,params,locals,cmds)::decls ->
        sub_prog_decl_eval decls ((updateenv(r,id,(Descr_Procedure(params,locals,cmds)))),s)


(* evaluation of actual parameter list *)
let rec eval_actual_params (e:aexp list) (r:env) (s:store) = match e with
      []        ->  [] 
    | esp::vl   ->  (eval_aexp esp r s)::(eval_actual_params vl r s)



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
                )
            | _ -> raise SYNTAX
    in
        if (List.length(input_values)!= List.length(parameters))
            then false
        else
            (check_types input_values parameters)


let rec assign_values (formals:param list) (actuals:value list) ((r:env), (s: store)) =
    match (formals,actuals) with
          [],[] -> (r,s)
        | Par(id,tipo)::forms,v::acts ->
            let (r',s') =
                assign_values forms acts (dec_eval [Dec(id,Basic(tipo))] r s)
            in
                (
                 match r'(id) with
                      Var(l) -> (r',updatemem(s',l,v))
                    | _ -> raise SYNTAX
                )
        | _ -> raise SYNTAX





(* execution of commands *)
let rec exec (c: cmd) (r: env) (s: store) = match c with
    Ass(i,e)        ->  let ret = eval_aexp e r s
                        in 
                        (
                         match i with
                              LVar(id)  -> (
                                            match r(id) with
                                              Var(l)    -> updatemem(s,l,ret)
                                            | _         -> raise SYNTAX
                                           )
                            | LVec(v,idx) -> (
                                              match r(v) with
                                                  Descr_Vector(Loc(vo),lb,ub) ->
                                                    let ValueInt(pos) = (eval_aexp idx r s)
                                                    in
                                                        if (pos >= lb && pos <= ub) then
                                                            updatemem(s,Loc(vo+pos),ret)
                                                        else
                                                            (
                                                             raise INDEX_OUT_OF_BOUNDS
                                                            )
                                                | _ -> raise SYNTAX
                                             )
                        )
    | Blk([])       ->  s
    | Blk(x::y)     ->  exec (Blk(y)) r (exec x r s)
    | Ite(b,c1,c2)  ->  if (eval_bexp b r s) then (exec c1 r s)
                        else (exec c2 r s)
    | While(b,c)    ->  if (not(eval_bexp b r s)) then s
                        else
                            let s'' = exec c r s
                            in (exec (While(b,c)) r s'')
    | For(i,valmin_exp,valmax_exp,c) 
                    ->  let valmin = eval_aexp valmin_exp r s
                        and update_counter l s =
                            match s(l) with
                              ValueInt(n) -> updatemem(s, l, ValueInt(n + 1))
                            | ValueFloat(f) -> updatemem(s, l, ValueFloat(f +. 1.0))
                        in 
                        (
                         match r(i) with
                              Var(l) -> 
                                (
                                let s0 = updatemem(s, l, valmin)
                                in
                                    let rec exec_for s =
                                        let s' = exec c r s
                                        in
                                            let ret = eval_bexp (LT(Ident(i), valmax_exp)) r s'
                                            in
                                                if (ret) then exec_for (update_counter l s')
                                                else s'
                                    in exec_for s0
                                )
                            | _ -> raise SYNTAX
                        )
    | Repeat(c,b)   ->  let s' = exec c r s
                        in
                            let ret = eval_bexp b r s'
                            in
                                if (ret) then s'
                                else (exec (Repeat(c,b)) r s')
    | Write(e)      ->  let ret = (eval_aexp e r s)
                        in
                        (
                         match ret with
                              ValueInt(op1) -> print_int(op1); print_string "\n";s
                            | ValueFloat(op1) -> print_float(op1); print_string "\n";s
                        )
    | PCall(id,input_exprs)
                    ->  let input_values = (eval_actual_params input_exprs r s)
                        in
                        (
                         match (r(id)) with
                              Descr_Procedure(params,locals,cmds) ->
                                if ((type_checking input_values params)) then
                                    exec_proc (r(id)) input_values r s
                                else
                                    raise PARAMETERS_DO_NOT_MATCH
                            | _ -> raise SYNTAX
                        )


(* execution of subprograms *)
and exec_proc (ee:env_entry) (input_values:value list) (r: env) (s: store) :store =
    let do_exec inVars locVars cmds (values:value list) (r: env) (s: store): store =
        let (r',s') = assign_values inVars values (r,s)
        in
            let (r'',s'') = dec_eval locVars r' s'
            in
                exec cmds r'' s''
    in
        match ee with
              Descr_Procedure(params,locals,cmds) ->
                do_exec params locals cmds input_values r s
            | _ -> raise SYNTAX


(* evaluation of programs *)
let run prog = 
    match prog with
        Program(vars,sub_progs,com) ->   let (r,s) = sub_prog_decl_eval sub_progs (dec_eval vars initenv initmem)
                                         in (exec com r s)
      | Null -> initmem
