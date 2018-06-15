type term =
  | TmVar of Lexing.position * string
  | TmAbs of Lexing.position * string * term (* ラムダ抽象。僕には関数定義に見える *)
  | TmApp of Lexing.position * term * term
