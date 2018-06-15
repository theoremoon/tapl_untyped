%{ 
  open Syntax
%}

%token LPAREN RPAREN
%token DOT LAMBDA
%token EOL
%token <string> STR

%nonassoc STR LPAREN LAMBDA
%left Apply

%start parse
%type <Syntax.term> parse

%%

parse: term EOL { $1 }

term:
  |LAMBDA v=STR DOT t=term { TmAbs($startpos, v, t) }
  |STR { TmVar($startpos, $1) }
  |LPAREN term RPAREN { $2 }
  |term term { TmApp($startpos, $1, $2) } %prec Apply

