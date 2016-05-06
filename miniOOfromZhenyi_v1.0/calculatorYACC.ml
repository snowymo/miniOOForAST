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
  | DOT
  | END
  | NUM of ( int )

open Parsing;;
let _ = parse_error;;
# 3 "calculatorYACC.mly"
 (* header *)
(* unused *)
let returnProc = -1;;
let procId : int ref = ref 0;;  (* indicates the fid of procedure *)
let varId : int ref = ref 0;;
type procList = (int * string) list;; (* proc fid and proc name *)
let prl = ref([] : procList) ;;
type varList = (string * ( string * int)) list ;; (* var name, for procedure using, 'y' for value and 'n' for future assignment *)
let vl = ref( [] : varList);;
let g_lSb = ref([] : procList);;  (*symbol list: fid, name*)

(* in use *)
let returnNum = -2;;
let g_stType : string ref = ref "nil";; (* indicates the type of var *)
let g_stPara : string ref = ref "nil";; (* indicates the name of the parameter *)

let cmdId : int ref = ref 0;;  (* indicates the fid of cmd, when cmd does not have a value then, it might be a part of a proc, so let's generate an fid for it *)

let procToRun : string ref = ref "nil";;
let isCmdInProc : bool ref = ref false;;    (* flag that if some par no show, it could be cmd in proc *)
let cmpop : string ref = ref "nil";;
  
  (* variable name + defined or not + type + value *)
type symbTable = (string * (string * (string * int))) list ;; (* if it is proc then the value is the cmdid of what to run first *)
let g_tSb = ref([] : symbTable) ;;

type procTable =  (string * (string * int))  list ;; (*proc name and parameter name and cmd idx*)
let g_tProc = ref([] : procTable) ;;

type procCntTable = (int * (int * int)) list;; (*proc fid * proc name cmd fid * parameter cmd fid *)
let g_tProcCnt = ref([] : procCntTable);;

type binOpTable = (int * (string * (int * int))) list ;;  (* fid ( op ( a,b))*)
let g_tBop = ref([] : binOpTable);;

type assignTable = (int * (string * int)) list ;; (* fid ( a = b) b is idx of cmd*)
let g_tAssign = ref([] : assignTable);;

type variableList = (int * string) list;; (* id and name *)
let g_lVar = ref([]: variableList);;  (*variable list: var id, name*)

type numList = (int * int) list;;(* cmd fid, value*)
let g_lNum = ref([] : numList);;

type cmdTable = (int * string) list ;; (*cmd fid, cmd type*)
let g_lCmd = ref([] : cmdTable);;

type condTable = (int * (int *( int * int))) list;; (*cmd fid, if cmd fid, then cmd fid, else cmd fid*)
let g_lCond = ref([] : condTable);;

let output x = (print_string(x); print_newline(); flush stdout; )
  ;;

  let rec except x l = match l with
  []   -> []
| h::t -> if (h = x) then t
            else h::(except x t)

let rec printNumList l = match l with
  [] -> print_newline(); flush stdout
  | h :: t -> print_string "num:"; print_int (fst h); print_string ":\t"; print_int (snd h); print_newline(); printNumList t

let rec printsb l = match l with
    [] -> print_newline();flush stdout
  | h::t -> print_string (fst h ^ "\tdef:" ^ (fst (snd h)) ^ "\ttype:" ^ (fst (snd (snd h))) ^ "\tvalue:"); print_int (snd (snd (snd h)));print_newline(); printsb t
  ;;

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
  |  h :: t -> print_string("cmd: "); print_int (fst h); print_string ("\top: " ^ fst(snd h) ^ "\t"); print_int (fst(snd(snd h))); print_string ("\t"); print_int(snd(snd(snd h)));print_newline();flush stdout; printBOP t
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

let rec printCmdList l = match l with
[] -> print_newline(); flush stdout
| h :: t -> print_string("cmd: "); print_int(fst h); print_string(":\t" ^ snd(h) ^ "\n"); printCmdList t
;;

let rec getIdByValue fid list= (
    match list with
    [] -> -1
    | h::t -> if(snd (h) = fid) then (fst (h)) else (getIdByValue fid t)
  );;

let addVar variable = 
  if(List.mem_assoc variable !g_tProc) then(
    printProc !g_tProc;
    procToRun := variable;
    snd (List.assoc variable !g_tProc)
    )
  else (
    if(List.mem_assoc variable !g_tSb) then(
        getIdByValue variable !g_lVar
      )
    else(
        let i = (generateId cmdId) in (
          g_lVar := (i,variable) :: !g_lVar;
          g_lCmd := (i, "var") :: !g_lCmd;
          printCmdList !g_lCmd;
          if( List.mem_assoc variable !g_tSb) then(
            
          )
          else(
            g_tSb := (variable, ("U",("nil",0))) :: !g_tSb
            );
          printsb !g_tSb;
          i
        )
    )
  );;

let getValueFromNum v =
  if(List.mem_assoc v !g_lNum) then(
      print_string (string_of_int (List.assoc v !g_lNum) ^ "\n");
      List.assoc v !g_lNum
    )
  else
    (print_string ("num not found.\n");-1)
;;

let getValInTuple t = 
  let (_t,_v) = t;
    in (if(_t = "nil") then -1 else getValueFromNum _v);;

let getvalue x =
   if (List.mem_assoc x !g_tSb) then 
   (if(fst (snd (List.assoc x !g_tSb)) = "proc") then
      (print_string "we need to run this proc.\n";
        procToRun := x;
        0)
    else
      getValInTuple (snd(List.assoc x !g_tSb))
    )
   else
     0;;



let getValueByCmd v = 
  if(List.mem_assoc v !g_lCmd) then
(
  match (List.assoc v !g_lCmd) with
    "num" -> (getValueFromNum v)
  | "var" -> (getvalue  (List.assoc v !g_lVar))
  | "true" -> (print_string("it is true.\n");1024)
  | "false" -> (print_string("it is false.\n");1023)
  | "field" -> (getvalue  (List.assoc v !g_lVar))
  | _  -> (print_string ("cmd type not found.\n");-1)
  )
  else
    (print_string ("cmd fid not found.\n");-1)
  ;;

let handleNum n = (
  let newid = generateId cmdId in (
    g_lNum := (newid,n) :: !g_lNum;
    printNumList !g_lNum;
    g_lCmd := (newid, "num") :: !g_lCmd;
    printCmdList !g_lCmd;
    newid
    )
  );;

let handleAssignNProc x t value = 
(
  if (List.mem_assoc x !g_tSb) then
          (match t with
          | "int"  -> (g_tSb := (x, ("D" ,(t, handleNum (getValueByCmd value)))) :: (except (x, (List.assoc x !g_tSb)) !g_tSb))
          | "proc"  -> (g_tSb := (x, ("D" ,(t, value))) :: (except (x, (List.assoc x !g_tSb)) !g_tSb))
          | _ -> print_string ("no match with " ^ t ^ "\t"); print_int value
          )
     else
      g_tSb := (x,("U",(t,handleNum (getValueByCmd value)))) :: !g_tSb;
  printsb !g_tSb;
  flush stdout;
  0
) ;;

let declareProc pid p y c = 
  g_lCmd := (pid, "proc") :: !g_lCmd;
  print_string ("now in declare proc.\n");
  if( List.mem_assoc pid !prl) then
    prl := (pid,p) :: (except (pid, (List.assoc pid !prl)) !prl)
  else
    prl := (pid, p) :: !prl;
  g_tProc := (p, (y,c)) :: !g_tProc
  ;;

let handleAssignInHelp cid = (
    match List.assoc cid !g_lCmd with
    "num" -> cid
    | "var" -> handleNum (getValueByCmd cid)
    | "field" -> (
      if( fst(List.assoc (List.assoc cid !g_lVar) !g_tSb) = "D") then 
        handleNum (snd(snd(List.assoc (List.assoc cid !g_lVar) !g_tSb)) )
      else (
        cid
        )
      )
    | _ -> cid
  );;

let handleAssignInProc x t value = 
(
  print_string (x ^ "\tcmd: ");
  print_int (value);
  print_string("\tin proc now.\n");
  flush stdout;
  if(t = "proc") then(
    let newx = (x, ("D",(t, value))) in
        (      
          g_tSb := newx :: (except (x,(List.assoc x !g_tSb)) !g_tSb);
          let genProcId = generateId cmdId in
              declareProc genProcId x !g_stPara value;
          printProc !g_tProc;
          isCmdInProc := false;
          genProcId
        ) 
    )
  else(
    let _cmdid = generateId cmdId in
    (g_tAssign := (_cmdid, (x,handleAssignInHelp value)) :: !g_tAssign; printASS !g_tAssign; print_newline();
      g_lCmd := (_cmdid, "ass") :: !g_lCmd;
      _cmdid)
  ) 
) ;;

let handleAssign x t v = 
(print_string (x ^ " = cmd:"); print_int (v);print_string ";\n"; 
   if( !isCmdInProc = false) then (
      handleAssignNProc x t v) 
   else(
      handleAssignInProc x t v)
  );;

let declareVar x=
  if( List.mem_assoc x !g_tSb) then(
    g_tSb := (x, ("D", snd(List.assoc x !g_tSb))) :: (except (x, (List.assoc x !g_tSb)) !g_tSb);
    print_string ("declareVar: found in sb");
    print_string (x);
    print_newline();
    )
    
  else(
    g_tSb := (x, ("U", ("nil", 0))) ::  !g_tSb;
    print_string ("declareVar: not found in sb");
    print_string (x);
    print_newline();
    )
    
    ;;

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



let getvalueProc x procSb =
   if (List.mem_assoc x !procSb) then 
      getValInTuple (List.assoc x !procSb)
   else
     (if (List.mem_assoc x !g_tSb) then 
        getValInTuple (snd(List.assoc x !g_tSb))
      else(
        print_string (x ^ " not declared,\n");
        0)
   );;

let printHandleProc pname para = (
  print_string ("ready to run:" ^ !procToRun ^ "\tcmd: ");
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

let handleNum n = (
  let newid = generateId cmdId in (
    g_lNum := (newid,n) :: !g_lNum;
    printNumList !g_lNum;
    g_lCmd := (newid, "num") :: !g_lCmd;
    printCmdList !g_lCmd;
    newid
    )
  );;

let addDirectCmd direct = (
    let i = generateId cmdId in(
        g_lCmd := (i,direct) :: !g_lCmd;
        printCmdList !g_lCmd;
        i)
  )
;;

let extractBop varid = (
  if(List.mem_assoc varid !g_lCmd) then(
    match List.assoc varid !g_lCmd with
    "num" -> let ret = List.assoc varid !g_lNum in (print_string ("num of " ^ (string_of_int varid) ^ " : " ^ (string_of_int ret) ^ "\n"); ret)
    | "var" -> let ret = (List.assoc (snd(snd(List.assoc (List.assoc varid !g_lVar) !g_tSb))) !g_lNum) in (print_string ("var:\t" ^ (string_of_int ret) ^ "\n");ret)
    | _ -> print_string ("not valid type.\n"); -1
    )
  else
    (print_string ("varid not in cmd list.\n");-1)
  )

let handleBop cid = (
  print_string ("binary operation:\t");
  let (_op,(_a,_b)) = (List.assoc cid !g_tBop) in
    (
      print_int (_a); print_string ("\t" ^ _op ^ "\t"); print_int (_b); print_newline();
      match _op with
      "+" -> (
              print_int (extractBop _a); print_string ("\t" ^ _op ^ "\t"); print_int (extractBop _b); print_newline();
               (handleNum ((extractBop _a) + (extractBop _b)))
              )
      |"-" -> (
              print_string ((string_of_int (extractBop _a)) ^ "\t" ^ _op ^ "\t" ^ (string_of_int (extractBop _b)) ^ "\n");
               (handleNum ((extractBop _a) - (extractBop _b)))
              )
      | "<" -> (
            print_string ((string_of_int (extractBop _a)) ^ "\t" ^ _op ^ "\t" ^ (string_of_int (extractBop _b)) ^ "\n"); 
            if((extractBop _a) < (extractBop _b)) then(
              addDirectCmd "true"
              )
            else(
              addDirectCmd "false"
              )
            )
      | _ -> (print_string ("operation not found\n");-1)
    )
    );;

let rec setProcParaHelp pnm pval = (
    if(List.assoc pval !g_lCmd = "binop") then
      handleBop pval
    else
      pval
  );;

let rec setProcPara pnm pval = (
  (*calculate pval first if it is not num*)
  let newpval = setProcParaHelp pnm pval in
    (
      if(List.mem_assoc pnm !g_tSb) then(
            g_tSb := (pnm, ("T", (fst (snd(List.assoc pnm !g_tSb)), newpval))) :: (except (pnm, List.assoc pnm !g_tSb) !g_tSb)
          )
      )
  );;

let rec handleCmd  cid = (
  match List.assoc cid !g_lCmd with
  "ass" -> (
      let (lft,rit) = (List.assoc cid !g_tAssign) in
      (* by default, rit is a cmid fid *)
        (print_string ("assignment:\t" ^ lft ^ " = cmd: " ^ (string_of_int rit) ^ "\n");
            let ret = handleCmd rit in
                if(List.mem_assoc lft !g_tSb) then
                  (g_tSb := (lft,("D",("int", ret))) :: (except (lft, (List.assoc lft !g_tSb)) !g_tSb);)
                else
                  g_tSb := (lft,("D",("int", ret))) :: !g_tSb;
                printsb !g_tSb;

                ret
              )
      )
  |"num" -> (
      getValueByCmd cid
    )
  |"binop" -> (
      let ret = (handleBop cid) in
                (print_int ret;
                  print_newline();
                          ret)
    )
  |"rec" ->(
      let (pid,paraid) = List.assoc cid !g_tProcCnt in(
          print_string ("handleCmd:" ^ (string_of_int (cid)) ^ " with paraid:" ^ (string_of_int paraid) ^ "\n");
          let stPName = List.assoc pid !g_lVar in
          (
            let (_para,_cmdid) = (List.assoc stPName !g_tProc) in
                    (
                      print_string ("para name:" ^ _para ^ "\trun cmd:" ^ (string_of_int _cmdid) ^ "\n");
                      setProcPara _para paraid;
                      printsb !g_tSb;
                      handleCmd _cmdid
                    )
          )
        )
    )
  |"cond" ->(
      let (ifid,(thenid,elseid)) = List.assoc cid !g_lCond in
      (
          let ifans = handleCmd ifid in
          (
              if (List.assoc ifans !g_lCmd) = "true" then(
                  print_string ("cond\ttrue:\t" ^ (string_of_int (thenid)) ^ "\n");
                  handleCmd thenid
                )
              else(
                  print_string ("cond\tfalse:\t" ^ (string_of_int (elseid)) ^ "\n");
                  handleCmd elseid
                )
            )
        )
    )
  |"field" -> (
      getvalue (List.assoc cid !g_lVar)
    )
  |_ -> print_string ("cmd not found in bop.\n");-1
  ) ;;

let rec setParaVal paraname paravalue = (
  if(List.mem_assoc paraname !vl) then(
      vl := (paraname,("y", paravalue)) :: (except (paraname, List.assoc paraname !vl) !vl)
    )
  ) ;;



let handleProc pname para = (
  (* run cmd pname and set para as the parameter *)
  if(List.mem_assoc !procToRun !g_tProc) then 
  (
    let (_para,_cmdid) = (List.assoc !procToRun !g_tProc) in
      (print_string ("para: " ^ _para ^ "=");
      print_int para;
      setProcPara _para para;
      print_string ("\nrun cmd: ");
      print_int _cmdid;
      print_newline();
      let ret = handleCmd _cmdid in(
          getValueByCmd ret
        )
      )
    )
  else(
    (* call the procedure before finished declaration*)
    let newid = generateId cmdId in(
        g_lCmd := (newid, "rec") :: !g_lCmd;
        printCmdList !g_lCmd;
        g_tProcCnt := (newid,(pname,para)) :: !g_tProcCnt;
        newid
      )
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

let checkParameter name list = 
if((List.mem_assoc name list)
  && (fst(snd(List.assoc name list)) <> "nil")) then (
    print_string (fst(snd(List.assoc name list)) ^ "\n");
    true
  )
  else(
      print_string (fst(snd(List.assoc name list)) ^ "\n");
      false
  )
  ;;

let checkPara paraid = (
  if(List.mem_assoc paraid !g_lCmd) then(
      let cmdtype = List.assoc paraid !g_lCmd in
      match cmdtype with
      "var" -> checkParameter (List.assoc paraid !g_lVar) !g_tSb
      |"num" -> true
      | _ -> print_string ("other type: " ^ cmdtype ^"\n");false
    )
  else(
    print_int paraid;
      print_string (" not in cmd.\n");
      false
    )
  );;

let handleHelp sta a = 
  if(checkParameter sta !g_tSb) then(
      let newid = generateId cmdId in (
       g_lNum := (newid,snd(snd(List.assoc sta !g_tSb))) :: !g_lNum;
       printNumList !g_lNum;
       g_lCmd := (newid, "num") :: !g_lCmd;
       printCmdList !g_lCmd;
       newid
      )
    )
  else(
       g_lCmd := (a, "var") :: (except (a, (List.assoc a !g_lCmd)) !g_lCmd);
       printCmdList !g_lCmd;
       a
      )
    ;;

let handleBinHelp inta = (
      if(List.mem_assoc inta !g_lCmd) then(
          let cmdtype = List.assoc inta !g_lCmd in
          match cmdtype with
          "var" -> (
              if (checkParameter (List.assoc inta !g_lVar) !g_tSb) then
                    (
                      let newid = generateId cmdId in(
                          print_string ((string_of_int inta) ^ " is  assigned.\n");
                          g_lNum := (newid,getValueByCmd inta) :: !g_lNum;
                          printNumList !g_lNum;
                          g_lCmd := (newid, "num") :: !g_lCmd;
                          printCmdList !g_lCmd;
                          newid
                      )
                    )
                else(
                    print_string ((string_of_int inta) ^ " is not already assigned.\n");
                    g_lCmd := (inta, "var") :: (except (inta, (List.assoc inta !g_lCmd)) !g_lCmd);
                    printCmdList !g_lCmd;
                    inta
                  )
            )
          |"num" -> inta
          | _ -> print_string ("wrong fid in handleBinHelp\n");-1
        )
      else(
        print_string("not in cmd list.\n");
        -1
        )
  );;

let handleBinop a op b = (
    printsb !g_tSb;
    
      if( (checkPara a) && (checkPara b)  ) then
        (
          print_string ("found all the parameter.\n");
          cal (getValueByCmd a)  op (getValueByCmd b)
        )
      else(
          print_string ("not found all the parameter.\n");
          let i = generateId cmdId in(
            g_tBop := (i,(op,(handleBinHelp a,handleBinHelp b))) :: !g_tBop;
            g_lCmd := (i, "binop") :: !g_lCmd;
            printCmdList !g_lCmd;
            printBOP !g_tBop;
            isCmdInProc := true;
            i
          )
        )
    
  ) ;;

let handleEqualHelper left right = (
  if(left = right) then
      (let i = generateId cmdId in(
        g_lCmd := (i,"true") :: !g_lCmd;
        printCmdList !g_lCmd;
        i)
      )
    else
      (let i = generateId cmdId in(
        g_lCmd := (i,"false") :: !g_lCmd;
        printCmdList !g_lCmd;
        i)
      )
  )
;;



let handleLesserHelper left right = (
  if(left < right) then
      (let i = generateId cmdId in(
        g_lCmd := (i,"true") :: !g_lCmd;
        printCmdList !g_lCmd;
        i))
    else
      (let i = generateId cmdId in(
        g_lCmd := (i,"false") :: !g_lCmd;
        printCmdList !g_lCmd;
        i)
      )
  )
;;

let handleCmpHelper left right op= (
  let (valLeft, valRight) = (getValueByCmd left, getValueByCmd right); in 
  (
    if(valLeft = -1) then 
      (
        print_string ("in handle lesser: not defined:\t");
        isCmdInProc := true;
        print_int left;
        print_newline();
        let i = generateId cmdId in(
            g_tBop := (i,(op,(left,right))) :: !g_tBop;
            g_lCmd := (i,"binop") :: !g_lCmd;
            printCmdList !g_lCmd;
            printBOP !g_tBop;
            i
          )
        )
      else(
          if(valRight = -1) then 
          (print_string ("in handle lesser: not defined:\t");
            isCmdInProc := true;
            print_int right;
            let i = generateId cmdId in(
                g_tBop := (i,(op,(left,right))) :: !g_tBop;
                g_lCmd := (i,"binop") :: !g_lCmd;
                printCmdList !g_lCmd;
                printBOP !g_tBop;
                i
              )
            )
          else(
            print_string ("in handle lesser: all defined:\t");
            match op with
              "<" -> handleLesserHelper left right
              | "=" -> handleEqualHelper left right
        )
      )
    )
  );;

let handleCmp left right = (
  match !cmpop with
  | "=" -> handleCmpHelper left right "="
  | "<" -> handleCmpHelper left right "<"
  );;



let handleIfElse cond thenClause elseClause = (
    let newid = generateId cmdId in (
        g_lCmd := (newid, "cond") :: !g_lCmd;
        g_lCond := (newid,(cond,(thenClause, elseClause))) :: !g_lCond;
        printCmdList !g_lCmd;
        newid
      )
  );;

let addObj o = (
    let newid = generateId cmdId in(
        g_lCmd := (newid, "obj") :: !g_lCmd;
        printCmdList !g_lCmd;
        g_lVar := (newid, o) :: !g_lVar;
        newid
      )
  );;



let handleField obj fld = (
  let newvar = obj^"."^fld in (
      if(List.mem_assoc newvar !g_tProc) then(
          printProc !g_tProc;
          procToRun := newvar;
          snd(List.assoc newvar !g_tProc)
        )
      else(
        if(List.mem_assoc newvar !g_tSb) then(
            getIdByValue newvar !g_lVar
          )
        else(
              let newid = generateId cmdId in (
                g_lCmd := (newid, "field") :: !g_lCmd;
                printCmdList !g_lCmd;
                g_lVar := (newid,obj^"."^fld) :: !g_lVar;
                if(List.mem_assoc newvar !g_tSb) then(
                  )
                else(
                  g_tSb := (newvar,("U",("nil",0))) :: !g_tSb
                  );
                printsb !g_tSb;
                newid
              )
          )
        )
    )
  );;

let handleFieldNotAssign fid ftype fvalue = (
    if(List.mem_assoc fid !g_lCmd) then(
        if(List.assoc fid !g_lCmd = "field") then (
            let fname = List.assoc fid !g_lVar in(
              if(List.mem_assoc fname !g_tSb) then
                (
                  match ftype with
                  | "int" -> g_tSb := (fname,("D",(ftype,handleNum (getValueByCmd fvalue)))) :: (except (fname,List.assoc fname !g_tSb) !g_tSb)
                  | "proc" -> g_tSb := (fname,("D",(ftype,fvalue))) :: (except (fname,List.assoc fname !g_tSb) !g_tSb)
                  | _ -> print_string("no match in field assignment with:" ^ ftype ^ "\n"))
                else(
                    g_tSb := (fname,("U",(ftype,handleNum (getValueByCmd fvalue)))) :: !g_tSb;
                  );
                printsb !g_tSb
              )
          )
        else(
            print_string ("It should be field.\n")
          )
      );
    0
  );;

let handleFieldInAssign fid ftype fvalue = (
    print_string((string_of_int fid) ^ "\t= cmd:" ^ (string_of_int fvalue) ^ "\n");
    if(ftype = "proc") then (
        let idproc = ((List.assoc fid !g_lVar), ("U",(ftype,fvalue))) in(
            g_tSb := idproc :: !g_tSb;
            let genProcId = generateId cmdId in
              declareProc genProcId (List.assoc fid !g_lVar) !g_stPara fvalue;
            printProc !g_tProc;
            isCmdInProc := false;
            genProcId
          )
      )
    else(
        let _cmdid = generateId cmdId in(
            g_tAssign := (_cmdid,((List.assoc fid !g_lVar),handleAssignInHelp fvalue)) :: !g_tAssign;
            printASS !g_tAssign;
            g_lCmd := (_cmdid,"ass") :: !g_lCmd;
            printCmdList !g_lCmd;
            _cmdid
          )
      )
  );;

let handleFieldAssign obj fld ftype fvalue = (
    print_string (obj ^ "." ^ fld ^ "= cmd:" ^ (string_of_int fvalue) ^ " as a " ^ ftype ^ "\n");
    let newid = handleField obj fld in (
      if(!isCmdInProc = false) then(
            handleFieldNotAssign newid ftype fvalue
          )
        else(
            handleFieldInAssign newid ftype fvalue
          )
      )
  );;

# 818 "calculatorYACC.ml"
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
  271 (* IFDEF *);
  272 (* ELSEDEF *);
  273 (* WHILEDEF *);
  274 (* TRUEDEF *);
  275 (* FALSEDEF *);
  276 (* THENDEF *);
  277 (* EQUAL *);
  278 (* LIGHTER *);
  279 (* MALLOCDEF *);
  280 (* DOT *);
  281 (* END *);
    0|]

let yytransl_block = [|
  267 (* VAR *);
  268 (* FIELD *);
  282 (* NUM *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\005\000\004\000\004\000\008\000\008\000\008\000\007\000\007\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\000\000"

let yylen = "\002\000\
\002\000\004\000\003\000\001\000\006\000\004\000\001\000\001\000\
\004\000\005\000\003\000\001\000\001\000\002\000\002\000\002\000\
\004\000\003\000\003\000\003\000\003\000\003\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\025\000\000\000\000\000\007\000\008\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\013\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\
\000\000\003\000\000\000\000\000\020\000\021\000\000\000\000\000\
\002\000\017\000\022\000\000\000\000\000\000\000\006\000\009\000\
\000\000\000\000\005\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\012\000\013\000\014\000\039\000\023\000"

let yysindex = "\004\000\
\092\255\000\000\003\255\252\254\004\255\103\255\022\255\000\000\
\000\000\009\255\039\255\000\000\000\000\119\255\246\254\032\255\
\046\255\049\255\034\255\000\000\000\000\007\255\040\255\053\255\
\000\000\092\255\246\254\246\254\246\254\246\254\246\254\013\255\
\066\255\092\255\105\255\058\255\246\254\246\254\000\000\105\255\
\064\255\000\000\048\255\048\255\000\000\000\000\128\255\246\254\
\000\000\000\000\000\000\013\255\013\255\060\255\000\000\000\000\
\013\255\105\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\134\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\052\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\031\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\073\255\
\139\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\041\255\063\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\054\255\067\255\000\000\000\000\000\000\
\088\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\232\255\224\255\000\000\000\000\250\255\000\000\000\000"

let yytablesize = 147
let yytable = "\022\000\
\019\000\042\000\050\000\005\000\001\000\015\000\017\000\054\000\
\032\000\049\000\027\000\028\000\029\000\030\000\018\000\008\000\
\027\000\028\000\029\000\030\000\043\000\044\000\045\000\046\000\
\047\000\059\000\016\000\037\000\038\000\024\000\052\000\053\000\
\023\000\025\000\023\000\023\000\023\000\023\000\023\000\023\000\
\026\000\057\000\018\000\033\000\018\000\018\000\023\000\034\000\
\018\000\018\000\023\000\023\000\023\000\029\000\030\000\023\000\
\018\000\036\000\035\000\040\000\018\000\018\000\018\000\041\000\
\019\000\018\000\019\000\019\000\048\000\051\000\019\000\019\000\
\055\000\015\000\011\000\058\000\004\000\000\000\019\000\000\000\
\011\000\011\000\019\000\019\000\019\000\000\000\016\000\019\000\
\011\000\010\000\000\000\000\000\011\000\011\000\011\000\010\000\
\010\000\011\000\000\000\000\000\000\000\000\000\003\000\010\000\
\004\000\005\000\006\000\010\000\010\000\010\000\000\000\000\000\
\010\000\019\000\007\000\003\000\005\000\008\000\005\000\006\000\
\020\000\021\000\027\000\028\000\029\000\030\000\031\000\007\000\
\008\000\000\000\008\000\027\000\028\000\029\000\030\000\000\000\
\056\000\023\000\023\000\023\000\023\000\023\000\022\000\022\000\
\022\000\022\000\022\000"

let yycheck = "\006\000\
\011\001\026\000\035\000\014\001\001\000\003\001\011\001\040\000\
\015\000\034\000\004\001\005\001\006\001\007\001\011\001\026\001\
\004\001\005\001\006\001\007\001\027\000\028\000\029\000\030\000\
\031\000\058\000\024\001\021\001\022\001\008\001\037\000\038\000\
\002\001\025\001\004\001\005\001\006\001\007\001\008\001\009\001\
\002\001\048\000\002\001\012\001\004\001\005\001\016\001\002\001\
\008\001\009\001\020\001\021\001\022\001\006\001\007\001\025\001\
\016\001\024\001\010\001\020\001\020\001\021\001\022\001\011\001\
\002\001\025\001\004\001\005\001\003\001\012\001\008\001\009\001\
\009\001\020\001\002\001\016\001\025\001\255\255\016\001\255\255\
\008\001\009\001\020\001\021\001\022\001\255\255\020\001\025\001\
\016\001\002\001\255\255\255\255\020\001\021\001\022\001\008\001\
\009\001\025\001\255\255\255\255\255\255\255\255\011\001\016\001\
\013\001\014\001\015\001\020\001\021\001\022\001\255\255\255\255\
\025\001\011\001\023\001\011\001\014\001\026\001\014\001\015\001\
\018\001\019\001\004\001\005\001\006\001\007\001\008\001\023\001\
\026\001\255\255\026\001\004\001\005\001\006\001\007\001\255\255\
\009\001\004\001\005\001\006\001\007\001\008\001\004\001\005\001\
\006\001\007\001\008\001"

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
  IFDEF\000\
  ELSEDEF\000\
  WHILEDEF\000\
  TRUEDEF\000\
  FALSEDEF\000\
  THENDEF\000\
  EQUAL\000\
  LIGHTER\000\
  MALLOCDEF\000\
  DOT\000\
  END\000\
  "

let yynames_block = "\
  VAR\000\
  FIELD\000\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 819 "calculatorYACC.mly"
                                           ( print_string "----prog------\n";flush stdout;  () )
# 981 "calculatorYACC.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 822 "calculatorYACC.mly"
                                        ( print_string ("----list----\n");flush stdout; 0)
# 989 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 823 "calculatorYACC.mly"
                                        ( print_string ("----list----\n");flush stdout; 0 )
# 997 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 824 "calculatorYACC.mly"
                                        ( print_string ("----list----\n");flush stdout; 0 )
# 1004 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 827 "calculatorYACC.mly"
                                                (print_string ("cmd\n"); flush stdout;0)
# 1013 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 :  string ) in
    Obj.repr(
# 828 "calculatorYACC.mly"
                                        (print_string ("cmd\n");flush stdout; 0)
# 1020 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 829 "calculatorYACC.mly"
                                        (  print_string ("cmd\n");flush stdout;0 )
# 1027 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 830 "calculatorYACC.mly"
                                        ( print_string "cmd\n";flush stdout; 0)
# 1034 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 833 "calculatorYACC.mly"
                              (print_string ("rec call\n");flush stdout;0)
# 1042 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 836 "calculatorYACC.mly"
                               ( print_string ("assignment\n");flush stdout;0 )
# 1051 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 837 "calculatorYACC.mly"
                     ( print_string ("assignment\n");flush stdout;0)
# 1059 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    Obj.repr(
# 840 "calculatorYACC.mly"
                          (print_string "true\n";flush stdout;0)
# 1065 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    Obj.repr(
# 841 "calculatorYACC.mly"
                          (print_string "false\n";flush stdout;0)
# 1071 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 842 "calculatorYACC.mly"
                       (print_string "cmp\n";flush stdout;0)
# 1079 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 845 "calculatorYACC.mly"
                     ( 0)
# 1086 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 846 "calculatorYACC.mly"
                     ( 0)
# 1093 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 849 "calculatorYACC.mly"
                              (0)
# 1101 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 850 "calculatorYACC.mly"
                             ( 0)
# 1109 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 851 "calculatorYACC.mly"
                             ( 0)
# 1117 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 852 "calculatorYACC.mly"
                             ( 0)
# 1125 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 853 "calculatorYACC.mly"
                             ( 0)
# 1133 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 854 "calculatorYACC.mly"
                              (0)
# 1141 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 855 "calculatorYACC.mly"
                           ( print_string ("var.\n");flush stdout; 0)
# 1148 "calculatorYACC.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  int ) in
    Obj.repr(
# 856 "calculatorYACC.mly"
                             (print_string ("num.\n");flush stdout;0)
# 1155 "calculatorYACC.ml"
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
# 857 "calculatorYACC.mly"
 (* trailer *)
# 1182 "calculatorYACC.ml"
