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
  | NUM of ( int )

open Parsing;;
let _ = parse_error;;
# 3 "calculatorYACC.mly"
 (* header *)

let returnProc = -1;;
let returnNum = -2;;
let stackTopType : string ref = ref "nil";; (* indicates the type of var *)
let stackTopPara : string ref = ref "nil";; (* indicates the name of the parameter *)
let procId : int ref = ref 0;;  (* indicates the id of procedure *)
let cmdId : int ref = ref 0;;  (* indicates the id of cmd, when cmd does not have a value then, it might be a part of a proc, so let's generate an id for it *)
let varId : int ref = ref 0;;
let procToRun : string ref = ref "nil";;
let isCmdInProc : bool ref = ref false;;    (* flag that if some par no show, it could be cmd in proc *)
  
type symbTable = (string * (string * int)) list ;; (* if it is proc then the value is the cmdid of what to run first *)

let sb = ref([] : symbTable) ;;

type procTable =  (string * (string * int))  list ;; (*proc name and parameter name and cmd idx*)
type procList = (int * string) list;; (* proc id and proc name *)
let prt = ref([] : procTable) ;;
let prl = ref([] : procList) ;;

type binOpTable = (int * (string * (string * string))) list ;;  (* id ( op ( a,b))*)
let bop = ref([] : binOpTable);;

type assignTable = (int * (string * int)) list ;; (* id ( a = b) b is idx of cmd*)
let asstbl = ref([] : assignTable);;

type varList = (string * ( string * int)) list ;; (* generate index , var name, for procedure using, 'y' for value and 'n' for future assignment *)
let vrlist = ref([]: procList);;
let vl = ref( [] : varList);;

let generateId refid = (
  refid := !refid + 1;
  !refid - 1
  );;

let rec printvarlist l = match l with
  [] -> print_newline() ; flush stdout
  |  h :: t -> print_string ((fst h) ^ "\t" ^ fst(snd h) ^ "\t"); 
    if(fst(snd h) = "y") then (print_int (snd(snd h))); 
    print_newline(); flush stdout; printvarlist t
  ;;

let rec printBOP l = match l with
  [] -> print_newline() ; flush stdout
  |  h :: t -> print_string("cmd: "); print_int (fst h); print_string ("\top: " ^ fst(snd h) ^ "\t" ^ fst(snd(snd h)) ^ "\t" ^ snd(snd(snd h)) ^ "\n");flush stdout; printBOP t
  ;;

let rec printASS l = match l with
  [] -> print_newline() ; flush stdout
  |  h :: t -> print_string("cmd: "); print_int (fst h); print_string ("\tleft: " ^ fst(snd h) ^ "\tequals cmd:"); print_int (snd(snd h)); printASS t
  ;;

let rec printProc l = match l with
  [] -> print_newline() ; flush stdout
  |  h :: t ->  print_string("proc: " ^ (fst h) ^ "\tpara: " ^ fst(snd h) ^ "\tcmd: "); print_int (snd(snd h)); printProc t
  ;;

let rec printProcList l = match l with
  [] -> print_newline() ; flush stdout
  |  h :: t ->  print_string("proc: "); print_int (fst h); print_string ("\t" ^ snd h ^ "\n"); printProcList t
  ;;

let addVar variable = 
  if(List.mem_assoc variable !prt) then(
    procToRun := variable;
    snd (List.assoc variable !prt)
    )
  else (
    (let i = (generateId varId) in (
      vrlist := (i,variable) :: !vrlist;
      if(List.mem_assoc variable !sb) then(
        vl := (variable,("y",snd (List.assoc variable !sb))) :: !vl;
      )
      else(
        vl := (variable,("n",0)) :: !vl;  
      );
    printvarlist !vl;
    i)
    )
    );;

let getValInTuple t = 
  let (_t,_v) = t;
    in _v;;

let getvalue x =
   if (List.mem_assoc x !sb) then 
   (if(fst (List.assoc x !sb) = "proc") then
      (print_string "we need to run this proc.\n";
        procToRun := x;
        0)
    else
      getValInTuple (List.assoc x !sb)
    )
   else
     0;;

let rec except x l = match l with
  []   -> []
| h::t -> if (h = x) then t
            else h::(except x t)

let handleAssignNProc x t value = 
(
  if (List.mem_assoc x !sb) then
          (match t with
          | "int"  -> (sb := (x, (t, value)) :: (except (x, (List.assoc x !sb)) !sb))
          | _ -> print_string ("no match with " ^ t ^ "\t"); print_int value)
     else
      print_string (x ^ " is not declared.\n");
  flush stdout;
  0
) ;;

let declareProc pid p y c = 
  if( List.mem_assoc pid !prl) then
    prl := (pid,p) :: (except (pid, (List.assoc pid !prl)) !prl)
  else
    prl := (pid, p) :: !prl;
  prt := (p, (y,c)) :: !prt
  ;;

let handleAssignInProc x t value = 
(
  print_string (x ^ "\tcmd: ");
  print_int (value);
  print_string("\tin proc now.\n");
  flush stdout;
  if(t = "proc") then(
    sb := (x, (t, value)) :: (except (x, (List.assoc x !sb)) !sb);
    let genProcId = generateId procId in
    declareProc genProcId x !stackTopPara value;
    printProc !prt;
    printProcList !prl;
    isCmdInProc := false;
    genProcId
    )
  else(
    let _cmdid = generateId cmdId in
    (asstbl := (_cmdid, (x,value)) :: !asstbl; printASS !asstbl; _cmdid)
  ) 
) ;;

let setvalue x t v =
  (print_string (x ^ " = "); print_int (v);print_string ";\n"; 
   if( !isCmdInProc = false) then (
      handleAssignNProc x t v) 
   else(
    handleAssignInProc x t v)
  );;

let declareVar x=
  if( List.mem_assoc x !sb) then
    sb := (x, ("nil", 0)) :: (except (x, (List.assoc x !sb)) !sb)
  else
    sb := (x, ("nil", 0)) ::  !sb;;

let equal x y =
   if (x = y) then 
     true
   else
     false;;

let lt x y =
   if (x < y) then 
     true
   else
     false;;

let output x = (print_string(x); print_newline(); flush stdout; )
  ;;

let rec printsb l = match l with
    [] -> print_newline();flush stdout
  | h::t -> print_string (fst h ^ "\ttype:" ^ (fst (snd h)) ^ "\tvalue:"); print_int (snd (snd h));print_newline(); printsb t
  ;;

let getvalueProc x procSb =
   if (List.mem_assoc x !procSb) then 
      getValInTuple (List.assoc x !procSb)
   else
     (if (List.mem_assoc x !sb) then 
        getValInTuple (List.assoc x !sb)
      else(
        print_string (x ^ " not declared,\n");
        0)
   );;

let printHandleProc pname para = (
  print_string (!procToRun ^ "\tcmd: ");
  print_int pname;
  print_string "\t";
  print_int para;
  print_newline();
  flush stdout;
  ) ;;

let getParaVal paraname= (
  if(List.mem_assoc paraname !vl) then(
      snd (List.assoc paraname !vl)
    )
  else
    -1
  );;

let handleBop cid = (
  print_string ("binary operation:\t");
  let (_op,(_a,_b)) = (List.assoc cid !bop) in
  (print_string (_a ^ "\t" ^ _op ^ "\t" ^ _b ^"\n");
    if(_op = "+") then(
      (getParaVal _a) + (getParaVal _b))
    else
      -1)
  );;

let rec handleCmd  cid = (
  (* based on cmid id we find the cmd *)
  if(List.mem_assoc cid !asstbl) then(
    let (lft,rit) = (List.assoc cid !asstbl) in
    (* by default, rit is a cmid id *)
      (print_string ("assignment:\t" ^ lft ^ " = cmd: ");
        print_int rit;
        print_newline();
        if(List.mem_assoc lft !sb) then(
          let ret = handleCmd rit in
              sb := (lft,("int", ret)) :: (except (lft, (List.assoc lft !sb)) !sb);
              printsb !sb;
              ret
              )
        else
          0
            )
    )
  else (
      if(List.mem_assoc cid !bop) then
        (let ret = (handleBop cid) in
          (print_int ret;
            print_newline();
                    ret))
      else
        0
    )
  ) ;;

let rec setParaVal paraname paravalue = (
  if(List.mem_assoc paraname !vl) then(
      vl := (paraname,("y", paravalue)) :: (except (paraname, List.assoc paraname !vl) !vl)
    )
  ) ;;

let handleProc pname para = (
  (* run cmd pname and set para as the parameter *)
  let (_para,_cmdid) = (List.assoc !procToRun !prt) in
    (print_string ("para: " ^ _para ^ "=");
    print_int para;
    setParaVal _para para;
    print_string ("\nrun cmd: ");
    print_int _cmdid;
    print_newline();
    let ret = handleCmd _cmdid in
    ret
    )
  );;

let cal a op b = (
  match op with
  | "+" -> a + b
  | "-" -> a - b
  | "*" -> a * b
  | "/" -> a / b
  | _ -> print_string ("unknown op.\n"); -1
  ) ;;



let handleBinop a op b = (
  print_int (a);
  let sta = (List.assoc a !vrlist ) in
    print_string ("\t"^sta^"\t"^op^"\t"); let stb = (List.assoc b !vrlist)  in
    print_int (b);
    print_string ("\t"^stb^"\n");
    printsb !sb;
  if( (List.mem_assoc sta !sb) && (List.mem_assoc stb !sb)  ) then
    (print_string ("found all the parameter.\n");
    cal (snd (List.assoc sta !sb))  op (snd (List.assoc stb !sb)))
  else(
    print_string ("not found all the parameter.\n");
    let i = generateId cmdId in(
      bop := (i,(op,(sta,stb))) :: !bop;
      printBOP !bop;
      isCmdInProc := true;
      i)
    )
  ) ;;

# 318 "calculatorYACC.ml"
let yytransl_const = [|
  257 (* EOL *);
  258 (* SEMICOLON *);
  259 (* ASSIGN *);
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* TIMES *);
  263 (* DIV *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* COLON *);
  269 (* VARDEF *);
  270 (* PROCDEF *);
    0|]

let yytransl_block = [|
  267 (* VAR *);
  268 (* FIELD *);
  271 (* NUM *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\005\000\004\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\002\000\001\000\001\000\004\000\003\000\
\004\000\003\000\003\000\003\000\003\000\002\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\016\000\017\000\
\001\000\000\000\005\000\006\000\000\000\015\000\014\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\012\000\013\000\000\000\
\009\000\007\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\011\000\012\000\013\000"

let yysindex = "\011\000\
\003\255\000\000\019\255\253\254\014\255\021\255\000\000\000\000\
\000\000\042\255\000\000\000\000\255\254\000\000\000\000\019\255\
\000\000\035\255\003\255\019\255\019\255\019\255\019\255\019\255\
\036\255\003\255\000\000\004\255\004\255\000\000\000\000\022\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\031\255\000\000\000\000\000\000\000\000\
\000\000\046\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\001\000\009\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\028\000\022\000\000\000\000\000\255\255"

let yytablesize = 278
let yytable = "\016\000\
\010\000\015\000\020\000\021\000\022\000\023\000\024\000\003\000\
\011\000\022\000\023\000\001\000\008\000\004\000\025\000\005\000\
\006\000\007\000\028\000\029\000\030\000\031\000\032\000\003\000\
\017\000\020\000\021\000\022\000\023\000\014\000\034\000\018\000\
\006\000\007\000\015\000\015\000\015\000\015\000\015\000\020\000\
\021\000\022\000\023\000\019\000\026\000\003\000\027\000\033\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\000\000\000\010\000\010\000\000\000\000\000\
\010\000\010\000\011\000\000\000\011\000\011\000\008\000\000\000\
\011\000\011\000\000\000\000\000\008\000\008\000"

let yycheck = "\003\001\
\000\000\003\000\004\001\005\001\006\001\007\001\008\001\005\001\
\000\000\006\001\007\001\001\000\000\000\011\001\016\000\013\001\
\014\001\015\001\020\000\021\000\022\000\023\000\024\000\005\001\
\011\001\004\001\005\001\006\001\007\001\011\001\009\001\011\001\
\014\001\015\001\004\001\005\001\006\001\007\001\008\001\004\001\
\005\001\006\001\007\001\002\001\010\001\000\000\019\000\026\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\004\001\005\001\255\255\255\255\
\008\001\009\001\002\001\255\255\004\001\005\001\002\001\255\255\
\008\001\009\001\255\255\255\255\008\001\009\001"

let yynames_const = "\
  EOL\000\
  SEMICOLON\000\
  ASSIGN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LPAREN\000\
  RPAREN\000\
  COLON\000\
  VARDEF\000\
  PROCDEF\000\
  "

let yynames_block = "\
  VAR\000\
  FIELD\000\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 323 "calculatorYACC.mly"
          ( (output "list"); print_int _1 ; print_newline(); flush stdout; () )
# 480 "calculatorYACC.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 326 "calculatorYACC.mly"
                        ( _3 )
# 488 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 327 "calculatorYACC.mly"
                        ( (output "cmd"); _1 )
# 495 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 330 "calculatorYACC.mly"
                          (output ("var " ^ _2 ); declareVar _2; printsb !sb; (getvalue _2))
# 502 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 331 "calculatorYACC.mly"
                                        (  _1 )
# 509 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 332 "calculatorYACC.mly"
                                        ( output "rec call: "; print_int _1; _1)
# 516 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 335 "calculatorYACC.mly"
                              (output ((string_of_int _1) ^ "(" ^ (string_of_int _3) ^ ")"); printHandleProc _1 _3; (handleProc _1 _3);)
# 524 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 338 "calculatorYACC.mly"
                     ( (setvalue _1 !stackTopType _3) )
# 532 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 341 "calculatorYACC.mly"
                              ((output ("proc " ^ _2 ^ ":")); stackTopType := "proc"; stackTopPara := _2; _4)
# 540 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 342 "calculatorYACC.mly"
                             ( handleBinop  _1 "+" _3 )
# 548 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 343 "calculatorYACC.mly"
                             ( _1 - _3 )
# 556 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 344 "calculatorYACC.mly"
                             ( _1 * _3 )
# 564 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 345 "calculatorYACC.mly"
                             ( _1 / _3 )
# 572 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 346 "calculatorYACC.mly"
                             ( - _2 )
# 579 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 347 "calculatorYACC.mly"
                           ( output _1; (addVar _1) )
# 586 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  int ) in
    Obj.repr(
# 348 "calculatorYACC.mly"
                             ( (output (string_of_int _1)); stackTopType := "int";_1 )
# 593 "calculatorYACC.ml"
               : int))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
;;
# 349 "calculatorYACC.mly"
 (* trailer *)
# 620 "calculatorYACC.ml"
