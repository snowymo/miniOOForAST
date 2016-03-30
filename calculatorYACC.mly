/* File calculatorYACC.mly */

%{ (* header *)

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

%} /* declarations */

%token EOL SEMICOLON ASSIGN PLUS /* lexer tokens */
%token MINUS TIMES DIV LPAREN RPAREN 
%token COLON
%token VAR FIELD VARDEF PROCDEF

%token < string > VAR
%token < string > FIELD
%token < int > NUM
%start prog                   /* the entry point */
%type <unit> prog  
%type <int> list
%type <int> cmd
%type <int> assign
%type <int> reccall
%type <int> expr
%left PLUS MINUS            /* lowest precedence */
%left TIMES DIV             /* medium precedence */
%nonassoc UMINUS           /* highest precedence */

%% /* rules */

prog :
    list  { (output "list"); print_int $1 ; print_newline(); flush stdout; () }
	
list :
    cmd SEMICOLON list  { $3 }
  | cmd                 { (output "cmd"); $1 } 
  
cmd :
    VARDEF VAR            {output ("var " ^ $2 ); declareVar $2; printsb !sb; (getvalue $2)}
  | assign                              {  $1 }
  | reccall                             { output "rec call: "; print_int $1; $1}

reccall :
    expr LPAREN expr RPAREN   {output ((string_of_int $1) ^ "(" ^ (string_of_int $3) ^ ")"); printHandleProc $1 $3; (handleProc $1 $3);}
  
assign :
    VAR ASSIGN expr  { (setvalue $1 !stackTopType $3) }
	
expr :
    PROCDEF VAR COLON cmd     {(output ("proc " ^ $2 ^ ":")); stackTopType := "proc"; stackTopPara := $2; $4}
  | expr PLUS expr           { handleBinop  $1 "+" $3 }
  | expr MINUS expr          { $1 - $3 }
  | expr TIMES expr          { $1 * $3 }
  | expr DIV expr            { $1 / $3 }
  | MINUS expr %prec UMINUS  { - $2 }
  | VAR                    { output $1; (addVar $1) }
  | NUM                      { (output (string_of_int $1)); stackTopType := "int";$1 }
%% (* trailer *)
