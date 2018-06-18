let num = ['0'-'9']

rule main = parse
| ' ' { main lexbuf }
| '(' { Parser.LPAREN }
| ')' { Parser.RPAREN }
| '.' { Parser.DOT }
| '\\' { Parser.LAMBDA }
| (['a'-'z'] | num)+ as id {
  Parser.STR(id)
}
| '=' { Parser.EQUAL }
| '\n' { Parser.EOL }
| eof { exit 1 }
