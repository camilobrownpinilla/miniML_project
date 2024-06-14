open CS51Utils ;;
open Absbook ;;
open Test_constants ;;
open Evaluation ;;

(*
================================================================================
                      TEST FILE FOR <evaluation.ml>   
================================================================================
*)
let empty = Env.empty()
let vts = Env.value_to_string ;;
let ets = Env.env_to_string ;;
(* Have to assume extend works b/c of abstraction barrier *)
let cENV = Env.extend empty cVARID1 (ref (Env.Val cNUMEXP)) ;;
let close_tests () = 
  Printf.printf "\n----------------- BEGIN close tests --------------------\n" ;
  unit_test ((vts (Env.close cFUNEXP empty)) = 
            (Printf.sprintf "((fun %s -> ~-%s), [])") cVARID1 cVARID1) 
  "close test" ;
  Printf.printf "----------------------- END TESTS ----------------------\n" ;
;;

let lookup_tests () = 
  Printf.printf "\n----------------- BEGIN lookup tests -------------------\n" ;
  unit_test (Env.lookup cENV cVARID1 = Env.Val cNUMEXP)
    "lookup test" ;
  Printf.printf "----------------------- END TESTS ----------------------\n" ;
;;

let extend_tests () =
  let open Env in
  let sp = Printf.sprintf in
  Printf.printf "\n----------------- BEGIN extend tests -------------------\n" ;

  unit_test (ets cENV 
    = (sp "[(%s, %s)]" cVARID1 randint_string)) 
    "Extend on empty" ;
  unit_test (ets (extend cENV cVARID1 (ref (Val cNUMEXP))) 
    = (sp "[(%s, %s)]" cVARID1 randint_string)) 
    "Extend: var already exists";
  unit_test (ets (extend cENV cVARID2 (ref (Val cBOOLEXP)))
    = (sp "[(%s, %s); (%s, true)]" cVARID1 randint_string cVARID2)) 
    "Extend: extend on existing env" ;
  Printf.printf "----------------------- END TESTS ----------------------\n" ;
;;

(*======================Can't continue testing due to time :(================ *)
let run_all () = 
  close_tests () ;
  lookup_tests () ;
  extend_tests () ;;

let _ = run_all () ;;