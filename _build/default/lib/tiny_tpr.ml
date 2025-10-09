type term =
	| Zero 
	| Succ of term 			(* succ(n) = n + 1*)
	| Var of string 		(* variables like x, y*)
	| Lam of string * term  	(* lambda x. body *)
	| App of term * term    	(* function application *)
	| Rec of term * term * term 	(* rec(base, step, n) *)

let rec eval term = 
	match term with
	| Zero -> Zero
	| Succ t -> Succ (eval t)
	| Var x -> failwith ("Unbound variable: " ^ x)
	| Lam (x, body) -> Lam (x, body)
	| App (Lam (x, body), arg) ->
		let evaluated_arg = eval arg in
		let rec substitute var_name replacement term =
			match term with
			| Var v -> if v = var_name then replacement else Var v
			| Zero -> Zero
			| Succ t -> Succ (substitute var_name replacement t)
			| Lam (v,t) -> Lam (v, substitute var_name replacement t)
			| App (t1, t2) -> App (substitute var_name replacement t1, substitute var_name replacement t2)
			| Rec (b, s, n) ->
				Rec (
					substitute var_name replacement b,
					substitute var_name replacement s,
					substitute var_name replacement n
				)
		in
		eval (substitute x evaluated_arg body)

	| App _ -> failwith "App: invalid function call"
	
	| Rec (_,_,_) -> failwith "Recursion not implemented yet"

