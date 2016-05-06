/* File calculatorYACC.mly */

%{ (* header *)
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

%} /* declarations */

%token EOL SEMICOLON ASSIGN PLUS /* lexer tokens */
%token MINUS TIMES DIV LPAREN RPAREN 
%token COLON
%token VAR FIELD VARDEF PROCDEF
%token IFDEF ELSEDEF WHILEDEF
%token TRUEDEF FALSEDEF THENDEF
%token EQUAL LIGHTER
%token MALLOCDEF
%token DOT LBRACKET RBRACKET
%token END
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
%type <int> cmpexpr
%type <int> boolexpr
%left PLUS MINUS            /* lowest precedence */
%left TIMES DIV             /* medium precedence */
%nonassoc UMINUS           /* highest precedence */

%% /* rules */

prog :
    list END                               { print_string "----prog------\n";flush stdout;  () }
	
list :
    VARDEF VAR SEMICOLON list           { print_string ("----list----\n");flush stdout; 0}
  | cmd SEMICOLON list                  { print_string ("----list----\n");flush stdout; 0 }
  | cmd                                 { print_string ("----list----\n");flush stdout; 0 } 
  
cmd :
    IFDEF boolexpr THENDEF cmd ELSEDEF cmd      {print_string ("cmd\n"); flush stdout;0}
  | MALLOCDEF LPAREN VAR RPAREN         {print_string ("cmd\n");flush stdout; 0}
  | assign                              {  print_string ("cmd\n");flush stdout;0 }
  | reccall                             { print_string "cmd\n";flush stdout; 0}

reccall :
    expr LPAREN expr RPAREN   {print_string ("rec call\n");flush stdout;0}
  
assign :
    VAR DOT FIELD ASSIGN expr  { print_string ("assignment\n");flush stdout;0 }
  | VAR ASSIGN expr  { print_string ("assignment\n");flush stdout;0}

boolexpr:
    TRUEDEF               {print_string "true\n";flush stdout;0}
  | FALSEDEF              {print_string "false\n";flush stdout;0}
  | expr cmpexpr       {print_string "cmp\n";flush stdout;0}

cmpexpr:
    EQUAL expr       { 0}
  | LIGHTER expr     { 0}
	
expr :
    PROCDEF VAR COLON cmd     {0}
  | expr PLUS expr           { 0}
  | expr MINUS expr          { 0}
  | expr TIMES expr          { 0}
  | expr DIV expr            { 0}
  | VAR DOT FIELD             {0}
  | VAR                    { print_string ("var.\n");flush stdout; 0}
  | NUM                      {print_string ("num.\n");flush stdout;0}
%% (* trailer *)
