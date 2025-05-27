# tiny_tpr
Mini proof assistant based on primitive recursive dependent type theory

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
	| App (_,_) -> failwith "Function application not implemented yet"
	| Rec (_,_,_) -> failwith "Recursion not implemented yet"
