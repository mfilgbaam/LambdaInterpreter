
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(*no es recursiva terminal*)
 let rec read_line_loop line = 
    	let input = read_line() in
    	 if (String.ends_with ~suffix:";;" input) 
    	 then   String.sub (input ^ " " ^ line) 0 (String.length(input ^ " " ^ line)-3)
    	 else   input ^ " " ^ read_line_loop line;;

let execute vctx tctx = function
  
  Eval tm ->
    let tyTm = typeof vctx tctx tm in
    let tm' = eval vctx tm in
    print_endline("-: " ^ string_of_ty tctx tyTm ^ " = " ^ string_of_term tctx tm');
    (vctx, tctx)

  | Bind (s, tm) ->
    let tyTm = typeof vctx tctx tm in
    let tm' = eval vctx tm in
    print_endline("-: " ^ string_of_ty tctx tyTm ^ " = " ^ string_of_term tctx tm');
    (addvbinding vctx s tm', addtbinding tctx s tyTm)
    
  | Type (s,ty) ->
    print_endline("-: type " ^ s ^ " = " ^ string_of_ty tctx ty);
    (vctx,(addtbinding tctx s ty))  
  ;;
    	 
let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_line_loop "")) in
      loop  (execute vctx tctx tm)
    with
       Lexical_error ->
         print_endline "lexical error";
         loop (vctx, tctx)
     | Parse_error ->
         print_endline "syntax error";
         loop (vctx, tctx)
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop (vctx, tctx)
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop (emptyvctx, empttyctx)
  ;;

top_level_loop ()
;;
