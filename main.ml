open Syntax

let position_to_string pos = 
  "file:" ^ pos.Lexing.pos_fname ^ ", line:" ^ string_of_int(pos.Lexing.pos_lnum) ^ ", col:" ^ string_of_int(pos.Lexing.pos_cnum)

(* 環境。名前と term を紐付ける *)
let empty_context : (string * term) list = []

exception LambdaError of string

let rec term_to_string term =
  match term with
  |TmVar(_, v) -> v
  |TmAbs(_, v, t) -> "(λ" ^ v ^ ". " ^ term_to_string t ^ ")"
  |TmApp(_, t1, t2) -> "(" ^ (term_to_string t1) ^ " " ^ (term_to_string t2) ^ ")"

let rec apply ctx t1 t2 =
  match t1 with
  |TmAbs(_, v, t) ->
      let ctx' = (v, t2)::ctx in
      eval ctx' t
  |_ -> raise (LambdaError "program error")
  
and eval ctx term =
  match term with
  |TmVar(_, v) ->
      begin
        try
          let v' = List.assoc v ctx in
          (ctx, v')
        with Not_found -> (ctx, term)
      end
  |TmApp(_, t1, t2) ->
      let _, t2' = eval ctx t2 in
      let ctx', t1' = eval ctx t1 in
      apply ctx' t1' t2'
  |_ -> (ctx, term)



let () =
  let t = Parser.parse Lexer.main (Lexing.from_channel stdin) in
  print_endline (term_to_string t);
  let _, r = eval empty_context t in 
  print_endline (term_to_string r);

