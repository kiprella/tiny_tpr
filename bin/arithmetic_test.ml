let test() = 
    let open Tiny_tpr in

    let two = make_nat 2 in
    let three = make_nat 3 in
    let addition_expr = App (App (add, two), three) in

    let result = eval [] addition_expr in
    let result_int = term_to_int result in

    print_endline ("2 + 3 = " ^ string_of_int result_int)