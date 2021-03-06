type token =
  | EOL
  | SEMICOLON
  | ASSIGN
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LPAREN
  | RPAREN
  | COLON
  | VAR of ( string )
  | FIELD of ( string )
  | VARDEF
  | PROCDEF
  | IFDEF
  | ELSEDEF
  | WHILEDEF
  | TRUEDEF
  | FALSEDEF
  | THENDEF
  | EQUAL
  | LIGHTER
  | MALLOCDEF
  | SKIPDEF
  | PARALLEL
  | DOT
  | END
  | LBRACKET
  | RBRACKET
  | ATOMDEF
  | NUM of ( int )

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
