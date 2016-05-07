/* File calculatorYACC.mly */

%{ (* header *)

(* in use *)
let g_stPara : string ref = ref "nil";; (* indicates the name of the parameter *)

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

type assignTable = (int * (int * int)) list ;; (* fid ( a = b) *)
let g_tAssign = ref([] : assignTable);;

type variableList = (int * string) list;; (* id and name *)
let g_lVar = ref([]: variableList);;  (*variable list: var id, name*)

type numList = (int * int) list;;(* cmd fid, value*)
let g_lNum = ref([] : numList);;

type cmdTable = (int * string) list ;; (*cmd fid, cmd type*)
let g_tCmd = ref([] : cmdTable);;

type condTable = (int * (int *( int * int))) list;; (*cmd fid, if cmd fid, then cmd fid, else cmd fid*)
let g_lCond = ref([] : condTable);;

(* new version *)
type heapTable = (int * ( string * (string * int))) list;; (* pos(var,(field,value)) *)
let g_tHeap = ref([] : heapTable);;
let g_tHeapAtom = ref([] : heapTable);;

type stackTable = (int * (string * int )) list;; (* stack id ( var name ( pos in heap)*)
let g_tStack = ref([] : stackTable);;

type closureTable = (int * (string * ( int * int))) list;; (* closure id ( parameter ( body cmd id , stack id)) *)
let g_tClo = ref([] :closureTable);;

type procedureTable = (int * ( int * int)) list;; (* proc/cmd id ( proc name cmd id, para cmd id) *)
let g_tProcedure = ref([] : procedureTable);;

type conditionTable = (int * (int *( int * int))) list;; (*cmd fid, if cmd fid, then cmd fid, else cmd fid*)
let g_tCond = ref([] : conditionTable);;

type whileTable = (int * (int * int)) list;; (* while id, cond id, cmd id *)
let g_tWhile = ref([] : whileTable);;

type seqTable = (int * (int * int)) list;; (* seq id, 1st id, 2nd id *)
let g_tSeq = ref([] : seqTable);;

type varDefTable = (int * string) list;;   (* def id, var id *)
let g_tVardef = ref([] : varDefTable);;

type atomTable = (int * ( int * int)) list;;
let g_tAtom = ref([] : atomTable);;
let g_tParallel = ref([] : atomTable);;

let g_nStackId : int ref = ref 0;;
let g_nHeapCmdId : int ref = ref 0;;
let g_bInProc : bool ref = ref false;;
let g_sPara : string ref = ref "";;
let g_iPara : int ref = ref (-1);;
let g_bAtom : bool ref = ref false;;
let g_AST : string ref = ref "";;
let g_idxAST : string ref = ref "";;

let rec printStackHelp l = (
    match l with
    [] -> print_newline()
    | h::t -> (print_string ("stack:\t" ^ (string_of_int (fst h)) ^ "\t" ^
                  (fst (snd h)) ^ "\t" ^ (string_of_int (snd (snd h))) ^ "\n"); 
        printStackHelp t)
  );;

let printStack () = (
    printStackHelp !g_tStack
  );;

let rec printHeapHelp l = (
    match l with
    [] -> print_newline()
    | h::t -> (print_string ("heap :\t" ^ (string_of_int (fst h)) ^ "\t" ^
                  (fst (snd h)) ^ "\t" ^ (fst (snd (snd h))) ^ "\t" ^(string_of_int (snd (snd (snd h)))) ^ "\n"); 
        printHeapHelp t)
  );;

let printHeap () = (
    print_string ("HEAP:\n");
    printHeapHelp !g_tHeap
  );;

let printHeapAtom () = (
    print_string ("HEAP ATOM:\n");
    printHeapHelp !g_tHeapAtom
  );;

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

let rec printBOP l = match l with
  [] -> print_newline() ; flush stdout
  |  h :: t -> print_string("cmd: "); print_int (fst h); print_string ("\top: " ^ fst(snd h) ^ "\t"); print_int (fst(snd(snd h))); print_string ("\t"); print_int(snd(snd(snd h)));print_newline();flush stdout; printBOP t
  ;;



let rec printCmdList l = match l with
[] -> print_newline(); flush stdout
| h :: t -> print_string("cmd: "); print_int(fst h); print_string(":\t" ^ snd(h) ^ "\n"); printCmdList t
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


let cal a op b = (
  match op with
  | "+" -> a + b
  | "-" -> a - b
  | "*" -> a * b
  | "/" -> a / b
  | _ -> print_string ("unknown op.\n"); -1
  ) ;;
(* new version *)
let rec checkVarHeap p_var p_field l= (
  (* check if the variable with field is inside the heap *)
    match l with
    | [] -> -1
    | h::t -> if((fst(snd h) = p_var) && (fst(snd(snd h)) = p_field)) then (
                fst h) 
              else (checkVarHeap p_var p_field t)
  );;

let pushStack p_id p_item = (
    if(List.mem_assoc p_id !g_tStack) then(
        g_tStack := (p_id, p_item) :: (except (p_id, (List.assoc p_id !g_tStack)) !g_tStack)
      )
    else(
        g_tStack := (p_id, p_item) :: !g_tStack
      )
  );;

let createVardef p_var  = (
    let newCmdId = generateId g_nHeapCmdId in(
        g_tCmd := (newCmdId, "vardef") :: !g_tCmd;
        printCmdList !g_tCmd;
        g_tVardef := (newCmdId, p_var) :: !g_tVardef;
        newCmdId
      )
  );;

let varDeclare p_var = (
    if(!g_bInProc) then(
        createVardef p_var 
      )
    else
    ((* update the stack and heap *)
        let (newHeapLoc, newStackId) = (generateId g_nHeapCmdId, generateId g_nStackId) in(
          g_tCmd := (newHeapLoc, "heap") :: !g_tCmd;
          g_tHeap := (newHeapLoc,(p_var,("val",-1))) :: !g_tHeap;
          printHeap ();
          pushStack newStackId (p_var, newHeapLoc);
          printStack ();
          newHeapLoc
        )
    )
  );;

let varEval p_var = (
      let p_pos = checkVarHeap p_var "val" !g_tHeap in(
          if(p_pos = -1) then(
              if(!g_bInProc && p_var = !g_sPara) then(
                  let newVarId = generateId g_nHeapCmdId in (
                      g_tCmd := (newVarId, "para") :: !g_tCmd;
                      g_lVar := (newVarId, p_var) :: !g_lVar;
                      newVarId
                  )
                )
              else(
                  print_string ("Not defined " ^ p_var ^ "\n");
                  let newHeapLoc = generateId g_nHeapCmdId in (
                      g_tHeap := (newHeapLoc, (p_var ,("V",-1))) :: !g_tHeap;
                      printHeap ();
                      -1
                  )
              )
            )
          else
            p_pos
        )
  );;

let numEval p_num = (
    let newCmdId = generateId g_nHeapCmdId in (
          g_tCmd := (newCmdId, "num") :: !g_tCmd;
          g_lNum := (newCmdId, p_num) :: !g_lNum;
          printNumList !g_lNum;
          printCmdList !g_tCmd;
          newCmdId
      )
  );;

(* TODO: what if the field is malloc in procedure so that the field is not visible in stack or heap*)
let fieldEval p_var p_field = (
    let p_pos = checkVarHeap p_var p_field !g_tHeap in (
        if(p_pos = -1) then(
            print_string ("Not defined " ^ p_var ^ "." ^ p_field ^ "\n");
            let newHeapLoc = generateId g_nHeapCmdId in (
                g_tHeap := (newHeapLoc, (p_var ,("V",-1))) :: !g_tHeap;
                printHeap();
                -1
            )
          )
        else(
            p_pos
          )
      )
  );;

let rec heapEvalHelp p_pos l = (
    match l with
    | [] -> print_string ("In heapEvalHelp: no valid pos.\n");-2
    | h::t -> if(fst h = p_pos) then (snd (snd (snd h))) else( heapEvalHelp p_pos t)
  );;

let createNum p_n = (
    let newCmdId = generateId g_nHeapCmdId in (
      g_lNum := (newCmdId,p_n) :: !g_lNum;
      printNumList !g_lNum;
      g_tCmd := (newCmdId, "num") :: !g_tCmd;
      printCmdList !g_tCmd;
      newCmdId
    )
  );;

let rec heapEvalCmd p_pos l = (
    match l with
    | [] -> print_string ("In heapEvalCmd: no cmd pos.\n");-1
    | h::t -> if(fst h = p_pos ) then (
                  if( snd h = "heap") then(
                      heapEvalHelp p_pos !g_tHeap
                    )
                  else(
                      p_pos
                  )
                )
              else(
                  heapEvalCmd p_pos t
                )
  );;

  let heapEval p_pos = (
      heapEvalCmd p_pos !g_tCmd
    );;

let createBinopHelp p_pos p_val = (
  (* one of para is -1 *)
    if(p_val <> -1) then
      p_val
    else
      p_pos
  );;

let createBinop p_f p_op p_s = (
    let newCmdId = generateId g_nHeapCmdId in (
        g_tBop := (newCmdId, (p_op, (p_f , p_s))) :: !g_tBop;
        g_tCmd := (newCmdId, "binop") :: !g_tCmd;
        newCmdId
      )
  );;

let heapEval2 p_pos = (
    if(p_pos <> -1) then(
      (* cmd id of answer which is a num *)
        heapEval p_pos
      )
    else(
        p_pos
      )
  );;

let binopEval p_fst p_op p_snd = (
    if(!g_bInProc) then(
          createBinop p_fst p_op p_snd
      )
    else(
          let (p_vf, p_vs) = (heapEval2 p_fst, heapEval2 p_snd) in(
            if(((p_vf > -1) && ((List.assoc p_vf !g_tCmd) = "num")) && 
                ((p_vs > -1) && ((List.assoc p_vs !g_tCmd) = "num") )) then(
                    (* calculate it and put it into g_lNum and return the cmdId *)
                    let p_ans = cal (List.assoc p_vf !g_lNum) p_op (List.assoc p_vs !g_lNum) in (
                        createNum p_ans
                      )
              )
            else(
                print_string ("in binopEval: wrong parameter.\n");
                -1
              )
          )
      )
    
  );;

let cmp a op b = (
    match op with
    | "=" -> if(a = b) then "true" else "false"
    | "<" -> if(a < b) then "true" else "false"
  );;

let createBool p_tf = (
    let newCmdId = generateId g_nHeapCmdId in (
        g_tCmd := (newCmdId, p_tf) :: !g_tCmd;
        newCmdId
      )
  );;

let bexprEval p_fst p_op p_snd = (
    if(!g_bInProc) then(
          createBinop p_fst p_op p_snd
      )
    else(
          let (p_vf, p_vs) = (heapEval2 p_fst, heapEval2 p_snd) in(
              if(((p_vf > -1) && (List.assoc p_vf !g_tCmd = "num")) && 
                  ((p_vs > -1) && (List.assoc p_vs !g_tCmd = "num") )) then(
                  (* if both of them have values, just calculate it and put it into g_lNum and return the cmdId *)
                  let p_ans = cmp (List.assoc p_vf !g_lNum) p_op (List.assoc p_vs !g_lNum) in (
                      createBool p_ans
                    )
                )
              else(
                  print_string ("In bexprEval: wrong parameter.\n");
                  -1
              )
          )
      )
    
  );;

let createAssign p_left p_right = (
    let newCmdId = generateId g_nHeapCmdId in(
        g_tAssign := (newCmdId, (p_left, p_right)) :: !g_tAssign;
        g_tCmd := (newCmdId, "ass") :: !g_tCmd;
        newCmdId
      )
  );;

let updateHeapAtom p_pos p_newval = (
    let (p_var, (p_field, p_val)) = List.assoc p_pos !g_tHeap in(
        if(List.mem_assoc p_pos !g_tHeapAtom) then(
            g_tHeapAtom := (p_pos, (p_var, (p_field, p_newval))) :: except (p_pos, List.assoc p_pos !g_tHeapAtom) !g_tHeapAtom
          )
        else(
            g_tHeapAtom := (p_pos, (p_var, (p_field, p_newval))) :: !g_tHeapAtom
          );
        print_string ("Updating heap atom.\n");
        printHeapAtom();
      )
  );;

let assignEval p_left p_right = (
    if( !g_bInProc ) then (
          createAssign p_left p_right
      )
    else(
        let p_v = heapEval2 p_right in(
              if((p_v > -1) && ((List.assoc p_v !g_tCmd = "num") || (List.assoc p_v !g_tCmd = "closure")) ) then(
                  let cnt = List.assoc p_left !g_tHeap in(
                    if(!g_bAtom) then(
                        print_string("In assignEval with ATOM: just run cmd.\n");
                        updateHeapAtom p_left p_v
                      )
                    else(
                        print_string("In assignEval without ATOM: just run cmd.\n");
                        g_tHeap := (p_left, (fst cnt, (fst (snd cnt), p_v))) :: except (p_left, cnt) !g_tHeap
                      );
                    print_string ("After assign.\n");
                    printHeap ();
                    p_left
                  )
                )
              else(
                  print_string ("In assignEval not found right.\n");
                  -1
                )
          ) 
      )
  );;

let procDeclare p_para p_cmdId = (
    let newCmdId = generateId g_nHeapCmdId in(
        g_tCmd := (newCmdId, "closure") :: !g_tCmd;
        printCmdList !g_tCmd;
        g_tClo := (newCmdId, (p_para, (p_cmdId, !g_nStackId))) :: !g_tClo;
        newCmdId
      )
  );;

let updateHeap p_pos p_newval = (
    let (p_var, (p_field, p_val)) = List.assoc p_pos !g_tHeap in(
        g_tHeap := (p_pos, (p_var, (p_field, p_newval))) :: 
        except (p_pos, (p_var, (p_field, p_val))) !g_tHeap;
        print_string ("Updating heap.\n");
        printHeap();
      )
  );;

let get p_numId = (
    List.assoc p_numId !g_lNum
  );;

let runCmp p_op p_l p_r = (
  print_string ("runcmp:" ^ (string_of_int (get p_l) ) ^ " " ^ p_op ^ " " ^ (string_of_int (get p_r)) ^ "\n");
    match p_op with
    | "<" -> if(get p_l < get p_r) then createBool "true" else createBool "false"
    | "=" -> if(get p_l = get p_r) then createBool "true" else createBool "false"
  );;

let getBool p_boolId = (
    match List.assoc p_boolId !g_tCmd with
    | "true"  -> true
    | "false" -> false
  );;

let rec popStack n = (
    if(n = 0) then (
        ()
        )
      else(
          g_nStackId := !g_nStackId - 1;
          g_tStack := except (!g_nStackId, (List.assoc !g_nStackId !g_tStack)) !g_tStack;
          printStack();
          if(n > 1) then
            (popStack (n-1))
      )    
  );;

let popStackTop () = (
  (* we pop parameter here *)
    popStack 1
  );;

let mergeHeapHelp p_newitem = (
    let p_heapid = fst p_newitem in(
      (* update heap with heapAtom *)
        g_tHeap := p_newitem :: 
        except (p_heapid, (List.assoc p_heapid !g_tHeap)) !g_tHeap;
        (* pop all heapAtom *)
        g_tHeapAtom := except (p_heapid, (List.assoc p_heapid !g_tHeapAtom)) !g_tHeapAtom;
      )
  )

let mergeHeaps () = (
    match !g_tHeapAtom with
    |[] -> ()
    | h::t -> (
            mergeHeapHelp h
      );
  );;

let rec runCmd2 p_cmdid p_stackId p_bAtom = (
    if(List.mem_assoc p_cmdid !g_tCmd) then
    (
      print_string ("Now run cmd:" ^ (string_of_int p_cmdid) ^ " " ^ List.assoc p_cmdid !g_tCmd ^ "\n");
      match List.assoc p_cmdid !g_tCmd with
        | "binop" -> let (p_op,(p_left, p_right)) = List.assoc p_cmdid !g_tBop in (
            printBOP !g_tBop;
            match p_op with
            | "+" ->  createNum ((List.assoc (runCmd2 p_left p_stackId  !g_bAtom) !g_lNum) + 
                    (List.assoc (runCmd2 p_right p_stackId  !g_bAtom) !g_lNum))
            | "-" ->  createNum ((List.assoc (runCmd2 p_left p_stackId  !g_bAtom) !g_lNum) - 
                    (List.assoc (runCmd2 p_right p_stackId  !g_bAtom) !g_lNum))
            | "<" ->  runCmp "<" (runCmd2 p_left p_stackId  !g_bAtom) (runCmd2 p_right p_stackId !g_bAtom)
            | "=" ->  runCmp "=" (runCmd2 p_left p_stackId !g_bAtom) (runCmd2 p_right p_stackId !g_bAtom)
          )
        | "heap" -> heapEval p_cmdid 
        | "ass" -> let p_ass = List.assoc p_cmdid !g_tAssign in(
              (* assign right to left *)
              if( p_bAtom) then(
                  updateHeapAtom (fst p_ass) (runCmd2 (snd p_ass) p_stackId !g_bAtom);-1
                )
              else(
                  updateHeap (fst p_ass) (runCmd2 (snd p_ass) p_stackId !g_bAtom);-1
                )
          )
        | "num" -> p_cmdid
        | "cond" -> let (p_if,(p_then,p_else)) = List.assoc p_cmdid !g_tCond in (
                if(getBool (runCmd2 p_if p_stackId !g_bAtom)) then(
                    runCmd2 p_then p_stackId !g_bAtom
                  )
                else(
                    runCmd2 p_else p_stackId !g_bAtom
                  )
              )
        | "true" -> createBool "true"
        | "false" -> createBool "false"
        | "closure" -> (
              let (newHeapLoc, newStackId) = (generateId g_nHeapCmdId, generateId g_nStackId) in (
                  (* push the parameter into stack *)
                  let (p_para,(p_id,p_stacktop)) = List.assoc p_cmdid !g_tClo in(
                      print_string("IN CLOSURE.\n");
                      g_tCmd := (newHeapLoc, "heap") :: !g_tCmd;
                      printCmdList !g_tCmd;
                      g_tHeap := (newHeapLoc, (p_para, ("val", !g_iPara))) :: !g_tHeap;
                      printHeap();
                      (* pop stack to what closure indicates *)
                      (*popStack (p_stacktop - !g_nStackId);*)
                      pushStack newStackId (p_para, newHeapLoc);
                      printStack ();
                      let (orist,ret) = (!g_nStackId, runCmd2 p_id p_stacktop !g_bAtom) in(
                          popStack (!g_nStackId - orist);
                          popStackTop();
                          ret
                      )
                  )
              )
          )
        | "para" -> (
              let p_paraname = List.assoc p_cmdid !g_lVar in(
                   let (nm,heapid) = List.assoc (!g_nStackId - 1) !g_tStack in(
                      if(nm <> p_paraname) then(
                          print_string ("WRONG: para should be at the stack top.\n");
                          -1
                        )
                      else(
                          heapEval heapid
                        )
                    )
                )
          )
        | "proc" -> (
            let (p_name, p_para) = List.assoc p_cmdid !g_tProcedure in(
                let p_cloId = heapEval p_name in(                    
                    g_iPara := (runCmd2 p_para !g_nStackId !g_bAtom);
                    runCmd2 p_cloId !g_nStackId  !g_bAtom
                  )
              )
          )
        | "while" -> (
            let (p_bid, p_id) = List.assoc p_cmdid !g_tWhile in(
                  let p_whileans = runCmd2 p_bid !g_nStackId !g_bAtom in(
                      if(getBool p_whileans) then(
                          runCmd2 p_id !g_nStackId !g_bAtom;
                          runCmd2 p_cmdid !g_nStackId  !g_bAtom
                        )
                      else(
                          0
                      )
                  )
              )
          )
        | "seq" ->(
            let (p_id1,p_id2) = List.assoc p_cmdid !g_tSeq in(
                runCmd2 p_id1 !g_nStackId !g_bAtom;
                runCmd2 p_id2 !g_nStackId !g_bAtom; 
                0
              )
          )
        | "vardef" ->(
            let p_var = List.assoc p_cmdid !g_tVardef in(
                let (newHeapLoc, newStackId) = (generateId g_nHeapCmdId, generateId g_nStackId) in(
                  g_tCmd := (newHeapLoc, "heap") :: !g_tCmd;
                  g_tHeap := (newHeapLoc,(p_var,("val",-1))) :: !g_tHeap;
                  printHeap ();
                  pushStack newStackId (p_var, newHeapLoc);
                  printStack ();
                  newHeapLoc
                )
            )
        )
        | "atom" ->(
          let (p_cmdid1, p_cmdid2) = List.assoc p_cmdid !g_tAtom in(
              g_bAtom := true;
              runCmd2 p_cmdid1 !g_nStackId !g_bAtom;
              runCmd2 p_cmdid2 !g_nStackId !g_bAtom;
              print_string ("Before Merging:\n");
              printHeap();
              mergeHeaps();
              print_string ("After Merging:\n");
              printHeap();  
              g_bAtom := false;
              0
            )
          )
        | "parallel" -> (
          let (p_cmdid1, p_cmdid2) = List.assoc p_cmdid !g_tParallel in(
              print_string ("current stack id:" ^ (string_of_int (!g_nStackId)) ^ "\n");
              if((!g_nStackId mod 2) = 1) then(
                  runCmd2 p_cmdid1 !g_nStackId !g_bAtom;
                  runCmd2 p_cmdid2 !g_nStackId !g_bAtom;
                )
              else(
                  runCmd2 p_cmdid2 !g_nStackId !g_bAtom;
                  runCmd2 p_cmdid1 !g_nStackId !g_bAtom;
                );
              0
            )
          )
      )
      else(
          -1
        )
  );;

let runCmd p_cmdid p_stackId = (
    runCmd2 p_cmdid p_stackId !g_bAtom
  );;

let rec printProcedureHelp l = (
    match l with
    |[] -> print_newline()
    | h::t -> print_string ("Proc:" ^ (string_of_int (fst h)) ^ "\t" ^ (string_of_int (fst (snd h)))^ "\t" ^ (string_of_int ( snd (snd h)))^ "\n")
  );;

let printProcedure () = (
    printProcedureHelp !g_tProcedure
  );;

let createProc p_name p_para = (
    let newCmdId = generateId g_nHeapCmdId in(
        g_tCmd := (newCmdId, "proc") :: !g_tCmd;
        printCmdList !g_tCmd;
        g_tProcedure := (newCmdId, (p_name, p_para)) :: !g_tProcedure;
        printProcedure ();
        newCmdId
      )
  );;

let runReccall p_name p_para = (
    (* p_name is heap pos of procedure name *)
    (* p_para is cmdid for "num" *)
    if(!g_bInProc) then(
        createProc p_name p_para
      )
    else(
        let p_cloId = heapEval p_name in(
          g_iPara := p_para;
          runCmd p_cloId !g_nStackId
        )
      )
  );;

let createCond p_if p_then p_else = (
    let p_condId = generateId g_nHeapCmdId in(
        g_tCond := (p_condId,(p_if,(p_then,p_else))) :: !g_tCond;
        g_tCmd := (p_condId, "cond") :: !g_tCmd;
        printCmdList !g_tCmd;
        p_condId
      )
  );;

let condEval p_if p_then p_else = (
    if(!g_bInProc) then(
        createCond p_if p_then p_else
      )
    else(
        let p_ifans = runCmd p_if !g_nStackId in(
            if(getBool p_ifans) then(
                runCmd p_then !g_nStackId
              )
            else(
                runCmd p_else !g_nStackId
              )
          )
      )
  );;

let createField p_name p_field = (
    let newHeapLoc = generateId g_nHeapCmdId in(
        g_tCmd := (newHeapLoc, "heap") :: !g_tCmd;
        g_tHeap := (newHeapLoc,(p_name,(p_field,-1))) :: !g_tHeap;
        printHeap();
        ()
        )
  );;

let createObj p_name = (
    (* create all fields for this variable *)
    let p_pos = checkVarHeap p_name "val" !g_tHeap in
    (
      if(p_pos = -1) then (
            print_string ("In createObj: cannot malloc because not defined.\n");
        )
        else(
          createField p_name "C";
          createField p_name "R";
          createField p_name "F";
          printStack ();
          ()
        )
      )
  );;

let skipEval () = (
    ()
  );;

let printWhileHelp l = (
    match l with
    [] -> print_newline();
    | h::t -> print_string ("While: " ^ (string_of_int (fst h)) ^ "\t" ^ (string_of_int (fst (snd h))) ^ "\t" ^ (string_of_int  (snd (snd h))) ^ "\n")
  );;

let printWhile () = (
    printWhileHelp !g_tWhile
  );;

let createWhile p_boolId p_cmdid = (
    let p_whileId = generateId g_nHeapCmdId in(
        g_tCmd := (p_whileId, "while") :: !g_tCmd;
        printCmdList !g_tCmd;
        g_tWhile := (p_whileId, (p_boolId, p_cmdid)) :: !g_tWhile;
        printWhile();
        p_whileId
      )
  );;

let rec whileEval p_boolId p_cmdid = (
    if(!g_bInProc) then(
        createWhile p_boolId p_cmdid
      )
    else(
        let p_whileans = runCmd p_boolId !g_nStackId in(
            if(getBool p_whileans) then(
                runCmd p_cmdid !g_nStackId;
                whileEval p_boolId p_cmdid
              )
            else(
                0
              )
          )
      )
  );;

let printSeqHelp l = (
    match l with
    [] -> print_newline()
    | h::t -> print_string ("Sequence: " ^ (string_of_int (fst h))^ "\t" ^ (string_of_int (fst (snd h))) ^ ";" ^ (string_of_int (snd (snd h))) ^ "\n")
  );;

let printSeq () = (
    printSeqHelp !g_tSeq
  )

let createSeq p_cmdid1 p_cmdid2 = (
    let newCmdId = generateId g_nHeapCmdId in(
        g_tCmd := (newCmdId, "seq") :: !g_tCmd;
        printCmdList !g_tCmd;
        g_tSeq := (newCmdId, (p_cmdid1, p_cmdid2)) :: !g_tSeq;
        printSeq();
        newCmdId
      )
  );;

let seqEval p_cmdid1 p_cmdid2 = (
    if(!g_bInProc) then(
        createSeq p_cmdid1 p_cmdid2
      )
    else(
        runCmd p_cmdid1 !g_nStackId;
        runCmd p_cmdid2 !g_nStackId;
        0
      )
  );;

let createAtom p_cmdid1 p_cmdid2 = (
    let newCmdId = generateId g_nHeapCmdId in(
        g_tCmd := (newCmdId, "atom") :: !g_tCmd;
        printCmdList !g_tCmd;
        g_tAtom := (newCmdId,(p_cmdid1, p_cmdid2)) :: !g_tAtom;
        newCmdId
      )
  );;


let atomEval p_cmdid1 p_cmdid2 = (
    if(!g_bInProc) then(
        createAtom p_cmdid1 p_cmdid2
      )
    else(
        print_string ("Before Merging:\n");
        printHeap();
        mergeHeaps();
        print_string ("After Merging:\n");
        printHeap();        
        0
      )
  );;

let createParallel p_cmdid1 p_cmdid2 = (
    let p_cmdid = generateId g_nHeapCmdId in(
        g_tParallel := (p_cmdid,(p_cmdid1, p_cmdid2)) :: !g_tParallel;
        g_tCmd := (p_cmdid, "parallel") :: !g_tCmd;
        printCmdList !g_tCmd;
        p_cmdid
      )
  );;

let parallelEval p_cmdid1 p_cmdid2 = (
    if(!g_bInProc) then(
        createParallel p_cmdid1 p_cmdid2
      )
    else(
        0
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
%token MALLOCDEF SKIPDEF PARALLEL
%token DOT END LBRACKET RBRACKET ATOMDEF
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
%type <string> procdef
%type <int> vardef
%type <unit> atom
%left PLUS MINUS            /* lowest precedence */
%left TIMES DIV             /* medium precedence */
%nonassoc UMINUS           /* highest precedence */

%% /* rules */

prog :
    list END                               { output "----prog------\n"; print_newline(); flush stdout; () }
	
list :
    cmd SEMICOLON list                      { print_string ("----list----\n");  $3 }
  | cmd                                     { print_string ("----list----\n"); $1 } 
  
cmd :
    vardef SEMICOLON cmd                    {$3}
  | IFDEF boolexpr THENDEF cmd ELSEDEF cmd      {condEval $2 $4 $6}
  | MALLOCDEF LPAREN VAR RPAREN             {output ("TODO MALLOC " ^ $3 ); createObj $3;0}
  | SKIPDEF                                   {skipEval;0}
  | WHILEDEF boolexpr cmd                   {whileEval $2 $3}
  | LBRACKET cmd SEMICOLON cmd RBRACKET     {seqEval $2 $4}
  | assign                                  {  $1 }
  | reccall                                 { $1}
  | atom LPAREN cmd SEMICOLON cmd RPAREN           {g_bAtom := false; atomEval $3 $5}
  | LBRACKET cmd PARALLEL cmd RBRACKET      {parallelEval $2 $4}

reccall :
    expr LPAREN expr RPAREN                 {output ((string_of_int $1) ^ "(" ^ (string_of_int $3) ^ ")"); (runReccall $1 $3)}
  
assign :
    VAR DOT FIELD ASSIGN expr               { assignEval (fieldEval $1 $3) $5}
  | VAR ASSIGN expr                         { (assignEval (varEval $1) $3) }

boolexpr:
    TRUEDEF                                {createBool "true"}
  | FALSEDEF                               {createBool "false"}
  | expr cmpexpr                            {bexprEval $1 !cmpop $2}

cmpexpr:
    EQUAL expr                              { cmpop := "="; $2}
  | LIGHTER expr                             { cmpop := "<"; $2}
	
expr :
    procdef cmd                             {g_bInProc := false; g_sPara := ""; procDeclare $1 $2}
  | expr PLUS expr                          { binopEval  $1 "+" $3 }
  | expr MINUS expr                         { binopEval  $1 "-" $3 }
  | expr TIMES expr                         { $1 * $3 }
  | expr DIV expr                           { $1 / $3 }
  | VAR DOT FIELD                            {output ("find field:" ^ $1 ^ "." ^ $3); fieldEval $1 $3}
  | VAR                                     { varEval $1 }
  | NUM                                    { (output ("find num:\t" ^ (string_of_int $1))); (numEval $1) }

vardef :
    VARDEF VAR                              {print_string ("find var:" ^ $2 ^ "\n"); varDeclare $2 ;0}

procdef :
    PROCDEF VAR COLON                       {print_string("proc def:\n"); g_bInProc := true; g_sPara := $2; $2}

atom :
    ATOMDEF                                  {g_bAtom := true}

%% (* trailer *)
