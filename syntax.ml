type term =
  | TmVar of Lexing.position * string
  | TmAbs of Lexing.position * string * term
  | TmApp of Lexing.position * term * term
