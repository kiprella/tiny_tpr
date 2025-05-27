let () = 
	let open Tiny_tpr in
	let t = Succ (Succ Zero) in
	let result = eval t in 
	match result with
	| Succ (Succ Zero) -> print_endline "Works"
	| _ -> print_endline "Error"

