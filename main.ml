open Syntax

exception LambdaError of string

type context =
  |Bind of string * term * context
  |Empty

let position_to_string pos = 
  "file:" ^ pos.Lexing.pos_fname ^ ", line:" ^ string_of_int(pos.Lexing.pos_lnum) ^ ", col:" ^ string_of_int(pos.Lexing.pos_cnum)

let rec term_to_string term =
  match term with
  |TmVar(_, v) -> v
  |TmAbs(_, v, t) -> "(λ" ^ v ^ ". " ^ term_to_string t ^ ")"
  |TmApp(_, t1, t2) -> "(" ^ (term_to_string t1) ^ " " ^ (term_to_string t2) ^ ")"

let rec ctx_to_string ctx =
  match ctx with
  |Bind(n, t, c) -> "[" ^ n ^ "->" ^ (term_to_string t) ^ (ctx_to_string c) ^ "]"
  |Empty -> ""

let rec cls_to_string cls =
  let t, c = cls in
  let tstr = term_to_string t in
  let cstr = ctx_to_string c in
  tstr ^ cstr

let rec merge_ctx c1 c2 =
  match c1 with
  |Bind (n, t, c) ->
      Bind(n, t, merge_ctx c c2)
  |Empty -> c2

let rec lookup ctx name global =
  match ctx with
  |Bind(n, t, c) ->
      begin
        if n = name then Some((t, c))
        else lookup c name global
      end
  |Empty ->
      try
        let t, c = List.assoc name global in
        Some((t, c))
      with Not_found -> None


let rec eval cls global debug =
  if debug then
    print_endline ("-->" ^ cls_to_string cls);

  let term, ctx = cls in
  match term with
  |TmVar(_, name) ->
      begin
        match lookup ctx name global with
        |Some((t, c)) -> eval (t, c) global debug
        |None -> (term, Empty)
      end
  |TmApp(_, f, a) ->
      begin
        let a = eval (a, ctx) global debug in
        let f = eval (f, ctx) global debug in
        apply f a global debug
      end
  |TmAbs(p, n, b) ->
      begin
        let b, ctx = eval (b, ctx) global debug in
        (TmAbs(p, n, b), ctx)
      end

and apply func arg global debug =
  let f, f_ctx = func in
  let a, a_ctx = arg in
  match f with
  |TmAbs(_, n, b) ->
      begin
        let ctx = Bind(n, a, merge_ctx a_ctx f_ctx) in
        eval (b, ctx) global debug
      end
  |_ -> 
      begin
        let ctx = merge_ctx a_ctx f_ctx in
        (TmApp(Lexing.dummy_pos, f, a), ctx)
      end


let interpret ch debug repl =
  let ctx = ref [] in
  let isend = ref false in
  while not !isend do
    try
      if repl then begin
        print_string "> ";
        flush stdout
      end;

      let stmt = Parser.parse Lexer.main (Lexing.from_channel ch) in
      match stmt with
      |Term(t) ->
          begin
            let r = eval (t, Empty) !ctx debug in 

            if repl then
              print_string "->";
            print_endline (cls_to_string r);
            if repl then
              print_newline ();
          end
      |Assign(_, name, t) ->
          begin
            let r, ctx' = eval (t, Empty) !ctx debug in 

            if repl then
              print_string "->";
            print_endline (cls_to_string (r, ctx'));
            if repl then
              print_newline ();

            ctx := (name, (r, ctx'))::!ctx
          end
      |Eof -> (print_endline "EOF"; isend := true)
      |_ -> ()
    with Parser.Error ->
      let p = (Lexing.from_channel ch).lex_curr_p in

      prerr_endline ("[error] syntax error at " ^ position_to_string p)
  done

let () =
  if Array.length Sys.argv = 1 then
    interpret stdin false true
  else if Array.length Sys.argv = 2 && Sys.argv.(1) = "-debug" then
    interpret stdin true true
  else
    begin
      let debug = ref false in
      let files = ref [] in
      for i = 1 to Array.length Sys.argv - 1 do
        if Sys.argv.(i) = "-debug" then
          debug := true
        else if Sys.argv.(i) = "-" then
          files := stdin::!files
        else
          files := (open_in Sys.argv.(i))::!files;
      done;

      List.iter (fun ch ->
        begin
          interpret ch !debug false;
          close_in ch
        end
      ) !files
    end
