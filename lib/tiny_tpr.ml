type term =
	| Zero 
	| Succ of term 			(* succ(n) = n + 1*)
	| Var of string 		(* variables like x, y*)
	| Lam of string * term  	(* lambda x. body *)
	| App of term * term    	(* function application *)
	| Rec of term * term * term 	(* rec(base, step, n) *)

let rec subst body x value =
        match body with
        | Zero -> Zero
        | Succ t -> Succ (subst t x value)
        | Var y -> if y = x then value else Var y
        | Lam (y, t) -> if y = x then Lam (y, t) else Lam (y, subst t x value)
        | App (t1, t2) -> App (subst t1 x value, subst t2 x value)
        | Rec (b, s, n) -> Rec (subst b x value, subst s x value, subst n x value)

let rec eval env term =
        match term with
        | Zero -> Zero
        | Succ t -> Succ (eval env t)
        | Var x -> (
            match List.assoc_opt x env with
            | Some v -> v
            | None -> failwith ("Unbound variable: " ^ x)
        )
        | Lam (x, body) -> Lam (x, body)
        | App (t1, t2) -> (
            let v1 = eval env t1 in
            let v2 = eval env t2 in
            match v1 with
            | Lam (x, body) -> eval env (subst body x v2)
            | _ -> failwith "Attempting to apply a non-function"
        )
        | Rec (base, step, n) -> (
            let v_base = eval env base in
            let v_step = eval env step in
            let rec iter m =
                match m with
                | Zero -> v_base
                | Succ m' ->
                        let prev = iter m' in
                        eval env (App (v_step, prev))
                | _ -> failwith "Non-numeral in recursion"
            in
            let vn = eval env n in
            iter vn
        )

let rec make_nat n =
    if n = 0 then Zero else Succ (make_nat (n - 1))

let add =
    Lam("m", Lam("n",
        Rec (Var "n", Lam ("x", Succ (Var "x")), Var "m")))

let mult = 
    Lam("m", Lam ("n",
        Rec (Zero, Lam("x", App (App (add, Var "m"), Var "x")), Var "n")))

let rec term_to_int = function
    | Zero -> 0
    | Succ t -> 1 + term_to_int t
    | _ -> failwith "Not a numeral"

