let () = 
	let open Tiny_tpr in
	let identity = Lam ("x", Var "x") in
	let applied = App(identity, Succ Zero) in
	let result = eval [] applied in
	match result with
	| Succ Zero -> print_endline "Function works"
	| _ -> print_endline "Error"
