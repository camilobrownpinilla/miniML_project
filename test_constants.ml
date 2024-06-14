open Expr ;;
open CS51Utils ;;
open Absbook ;;
(*
================================================================================
                          CONSTANTS FOR TESTING   
================================================================================
*)

(* Generates variables from range [0,100) and [100, 200), respectively *)
let cVARID1 = "x" ^ string_of_int(Random.int 100)
let cVARID2 = "x" ^ string_of_int(Random.int 100 + 100)
let cRANDINT = Random.int 230

(* ---------------- Test expressions to be used in tests ---------------------*)
let cVAREXP = Var cVARID1 ;;
let cVAREXP_ALT = Var cVARID2 ;;
let cNUMEXP = Num cRANDINT ;;
let cBOOLEXP = Bool true ;;
let cUNOPEXP = Unop (Negate, cVAREXP) ;;
let cBINOPEXP = Binop (Plus, cVAREXP, cVAREXP_ALT) ;;
let cCONDEXP = Conditional (cBOOLEXP, cBINOPEXP, cUNOPEXP) ;;
let cFUNEXP = Fun (cVARID1, cUNOPEXP) ;;
let cLETEXP = Let (cVARID1, cNUMEXP, cBINOPEXP) ;;
let cLETRECEXP = Letrec (cVARID1, cNUMEXP, cBINOPEXP) ;;
let cAPP = App (cVAREXP, cVAREXP_ALT) ;;

(* Useful for tests *)
let randint_string = string_of_int cRANDINT ;;

(*--------------------- Expressions for subst tests --------------------------*)
let cVARIDSUB = "x" ^ string_of_int(Random.int 100 + 200) ;;
let cVAREXPSUB = Var cVARIDSUB ;;
let cUNOPSUB = Unop (Negate, cVAREXPSUB) ;;
let cBINOPSUB = Binop (Plus, cVAREXPSUB, cVAREXP_ALT) ;;
let cAPPSUB = App (cVAREXPSUB, cVAREXP_ALT) ;;
let cCONDSUB = Conditional (cBOOLEXP, cBINOPSUB, cUNOPSUB) ;;
let cFUNEXPSUB = Fun (cVARID1, cUNOPSUB) ;;
let cLETEXPSUB = Let (cVARID1, cNUMEXP, cBINOPSUB) ;;
