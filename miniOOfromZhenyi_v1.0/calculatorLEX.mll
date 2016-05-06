(* File calculatorLEX.mll *)
{
open CalculatorYACC;; (* Type token defined in CalculatorYACC.mli *)
exception Error;;
exception Eof;;

}
rule token = parse
    [' ' '\t' '\n'] { token lexbuf } (* skip blanks and tabs *)
  | '#'         {END}
  | "var"      {VARDEF}
  | "proc"      {PROCDEF}
  | "if"        {IFDEF}
  | "then"      {THENDEF}
  | "else"      {ELSEDEF}
  | "while"     {WHILEDEF}
  | "true"      {TRUEDEF}
  | "false"     {FALSEDEF}
  | "malloc"     {MALLOCDEF}
  | (['a'-'z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as var
               { VAR var }
  | (['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as fld
               { FIELD fld }
  | ['0'-'9']+ as num
               { NUM (int_of_string num) }
  | ';'        { SEMICOLON }
  | ':'        { COLON}
  | '.'         {DOT}
  | "=="        {EQUAL}
  | "<"         {LIGHTER}
  | '='        { ASSIGN }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { TIMES }
  | '/'        { DIV }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | "{"         {LBRACKET}
  | "}"         {RBRACKET}
  | eof        { raise Eof }
  | _ as c     { print_string "incorrect lexem:"; print_char c; 
                 print_string ": code :"; print_int (int_of_char c); 
                 print_newline (); flush_all (); raise Error }
