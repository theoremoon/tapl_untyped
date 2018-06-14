open Syntax

let position_to_string pos = 
  "file:" ^ pos.Lexing.pos_fname ^ ", line:" ^ string_of_int(pos.Lexing.pos_lnum) ^ ", col:" ^ string_of_int(pos.Lexing.pos_cnum)


let rec term_to_string term =
  match term with
  |TmVar(_, v) -> v
  |TmAbs(_, v, t) -> "λ" ^ v ^ ". " ^ term_to_string t
  |TmApp(_, t1, t2) -> (term_to_string t1) ^ " " ^ (term_to_string t2)

let () =
  let t = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  print_endline (term_to_string t);
