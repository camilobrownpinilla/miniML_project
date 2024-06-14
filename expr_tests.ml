open Expr ;;
open CS51Utils ;;
open Absbook ;;
open Test_constants ;;

(*
================================================================================
                            TEST FILE FOR <expr.ml>   
================================================================================
*)

let exp_to_concrete_string_test () =
  let etc = exp_to_concrete_string in
  let unop_string = "~-" ^ cVARID1 in
  let binop_string = (cVARID1 ^ " + " ^ cVARID2) in
  Printf.printf "\n--------------- BEGIN exp_to_concrete tests ------------\n" ;
  unit_test ((etc cVAREXP) = cVARID1) "var test" ;
  unit_test ((etc cNUMEXP) = randint_string) "num test" ;
  unit_test ((etc cBOOLEXP) = "true") "bool test" ;
  unit_test ((etc cUNOPEXP) = unop_string) "Unop test" ;
  unit_test ((etc cBINOPEXP) = binop_string) "Binop test" ;
  unit_test ((etc cCONDEXP) = 
    Printf.sprintf "if true then %s else %s" binop_string unop_string) 
    "Cond test" ;
  unit_test ((etc cFUNEXP) = "(fun " ^ cVARID1 ^ " -> " ^ unop_string ^ ")")
            "Fun test" ;
  unit_test ((etc cLETEXP) = 
    "let " ^ cVARID1 ^ " = " ^ randint_string ^ " in " ^ binop_string)
    "Let test" ;
  unit_test ((etc cLETRECEXP) = 
  "let rec " ^ cVARID1 ^ " = " ^ (string_of_int cRANDINT) ^ " in " ^ binop_string)
  "Letrec test" ;
  unit_test ((etc cAPP) = cVARID1 ^ " " ^ cVARID2) "App test" ;
  Printf.printf "----------------------- END TESTS ----------------------\n" ;
;;

let exp_to_abstract_string_test () =
  let eta = exp_to_abstract_string in
  let print = Printf.sprintf in 
  let varexp_string  = print "Var(%s)" cVARID1 in
  let num_string = print "Num(%s)" randint_string in
  let varexpalt_string = print "Var(%s)" cVARID2 in
  let unop_string = print "Unop(Negate, %s)" varexp_string in
  let binop_string = print "Binop(Plus, %s, %s)" varexp_string varexpalt_string in

  Printf.printf "\n--------------- BEGIN exp_to_abstract tests ------------\n" ;
  unit_test ((eta cVAREXP) = varexp_string) "Var test" ;
  unit_test ((eta cNUMEXP) = num_string) "Num test" ;
  unit_test ((eta cBOOLEXP) = "Bool(true)") "Bool test" ;
  unit_test ((eta cUNOPEXP) = unop_string) "Unop test" ;
  unit_test ((eta cBINOPEXP) = binop_string) "Binop test" ;
  unit_test ((eta cCONDEXP) = 
    print "Conditional(Bool(true), %s, %s)" binop_string unop_string) 
    "Cond test" ;
  unit_test ((eta cFUNEXP) = 
    print "Fun(%s, %s)" cVARID1 unop_string) 
    "Fun test" ;
  unit_test ((eta cLETEXP) = 
    print "Let(%s, %s, %s)" cVARID1 num_string binop_string) 
    "Let test" ;
  unit_test ((eta cLETRECEXP) = 
    print "Letrec(%s, %s, %s)" cVARID1 num_string binop_string) 
    "Letrec test" ;
  unit_test ((eta cAPP) = 
    print "App(%s, %s)" varexp_string varexpalt_string) 
    "App test" ;
  Printf.printf "----------------------- END TESTS ----------------------\n" ;
;;

let free_vars_test () =
  let sv = same_vars and fv = free_vars and vol = vars_of_list in
  let empty = vol [] and var1 = vol [cVARID1] and var2 = vol [cVARID2] in
  let varboth = vol [cVARID1; cVARID2] in
  Printf.printf "\n---------------- BEGIN free_vars tests -----------------\n" ;
  unit_test ((sv (fv cVAREXP) var1) = (sv (fv cVAREXP_ALT) var2 )) 
    "Var test" ;
  unit_test (sv (fv cNUMEXP) empty) 
    "Num test" ;  
  unit_test (sv (fv cBOOLEXP) empty)
    "Bool test" ;
  unit_test (sv (fv cUNOPEXP) var1)
    "Unop test" ;
  unit_test (sv (fv cBINOPEXP) varboth)
    "Binop test" ;
  unit_test (sv (fv cCONDEXP) varboth)
    "Cond test" ;
  unit_test (sv (fv cLETEXP) var2)
    "Let test" ;
  unit_test (sv (fv cLETRECEXP) var2)
    "Letrec test" ;
  unit_test (sv (fv cAPP) varboth)
    "App test" ;
  Printf.printf "----------------------- END TESTS ----------------------\n" ;
;;

(* Test should show that new_varname does not adhere to Leibniz' principle of
   subsitution. That is, two instances of a call to the same function should
   never be equal. That way, new_varname always generates a unique varid. *)
let new_varname_test () = 
  Printf.printf "\n--------------- BEGIN new_varname tests ----------------\n" ;
  unit_test ((new_varname () <> new_varname ())) "varname test" ;
  Printf.printf "----------------------- END TESTS ----------------------\n" ;
;;

let subst_test () = 
  Printf.printf "\n------------------ BEGIN subst tests -------------------\n" ;
  unit_test (subst cVARID1 cVAREXP cNUMEXP = cNUMEXP) "Num sub" ;
  unit_test (subst cVARID1 cVAREXP cBOOLEXP = cBOOLEXP) "Bool sub" ;
  unit_test (subst cVARID1 cVAREXP cVAREXP = cVAREXP) "Var for self" ;
  unit_test (subst cVARID1 cVAREXP_ALT cVAREXP = cVAREXP_ALT) "Var for var" ;
  unit_test (subst cVARID1 cVAREXPSUB cUNOPEXP = cUNOPSUB) "Unop sub" ;
  unit_test (subst cVARID1 cVAREXPSUB cBINOPEXP = cBINOPSUB) "Binop sub" ;
  unit_test (subst cVARID1 cVAREXPSUB cAPP = cAPPSUB) "App sub" ;
  unit_test (subst cVARID1 cVAREXPSUB cCONDEXP = cCONDSUB) "Cond sub" ;
  unit_test (subst cVARID1 cVAREXP cFUNEXP = cFUNEXP) "Fun: Same var" ;
  unit_test (subst cVARID1 cVAREXPSUB cFUNEXP = cFUNEXP) "Fun: Not in FV" ;
  unit_test (subst cVARID1 cVAREXP cLETEXP = cLETEXP) "Let: Same var" ;
  unit_test (subst cVARID1 cVAREXPSUB cLETEXP = cLETEXP) "Let: Not in FV" ;
  Printf.printf "----------------------- END TESTS ----------------------\n" ;
;;

let run_all () = 
  exp_to_concrete_string_test () ;
  exp_to_abstract_string_test () ;
  free_vars_test () ;
  new_varname_test () ;
  subst_test () ;;

let _ = run_all () ;;