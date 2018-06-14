%{ 
  open Syntax
%}

%token LPAREN RPAREN
%token DOT LAMBDA
%token EOL

%token <string> STR

%start toplevel
%type <Syntax.term> toplevel

%%

toplevel: Term EOL { $1 }

Term:
  | LPAREN t=Term RPAREN { t }
  | LAMBDA v=STR DOT t=Term { TmAbs($startpos, v, t) }
  | t1=Term t2=Term { TmApp($startpos, t1, t2) }
  | v=STR { TmVar($startpos, v) }
