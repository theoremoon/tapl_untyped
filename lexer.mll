rule main = parse
| ' ' { main lexbuf }
| '(' { Parser.LPAREN }
| ')' { Parser.RPAREN }
| '.' { Parser.DOT }
| '\\' { Parser.LAMBDA }
| ['a'-'z']+ as id {
  Parser.STR(id)
}
| '=' { Parser.EQUAL }
| '\n' { Parser.EOL }
| eof { exit 1 }
