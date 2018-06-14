rule main = parse
| ' ' { main lexbuf }
| '(' { Parser.LPAREN }
| ')' { Parser.RPAREN }
| '.' { Parser.DOT }
| '\\' { Parser.LAMBDA }
| ['a'-'z'] as id {
  Parser.STR(String.make 1 id)
}
| '\n' { Parser.EOL }
| eof { exit 1 }
