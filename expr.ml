(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | Sin
  | Cos
  | Not
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Div
  | Exp
  | Equals
  | NotEquals
  | LessThan
  | LessThanEq
  | GreaterThan
  | GreaterThanEq
  | And
  | Or
  | Concat
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float                       (* floats *)
  | Char of char                         (* chars *)
  | String of string                     (* strings *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  let open SS in
  match exp with
  | Num _ | Bool _ | Float _ | Char _ | String _ | Raise | Unassigned -> empty
  | Var x -> singleton x
  | Unop (_, p) -> free_vars p
  | Binop (_, p, q) | App (p, q) -> 
    union (free_vars p) (free_vars q)
  | Fun (v, p) -> 
    diff (free_vars p) (singleton v)
  | Let (v, p, q) -> 
    union (diff (free_vars q) (singleton v)) (free_vars p)
  | Letrec (v, p, q) ->
    diff (union (free_vars p) (free_vars q)) (singleton v) 
  | Conditional (e1, e2, e3) -> 
    union (free_vars e1) (free_vars e2) |> union (free_vars e3);;
  

(* Define 'gensym' to generate a unique integer suffix for 'new_varname' *)
let gensym : string -> string =
  let suffix = ref 0 in
  fun str -> let symbol = str ^ string_of_int !suffix in
              suffix := !suffix + 1;
              symbol ;;

(* new_varname () -- Returns a freshly minted `varid` with prefix
   "var" and a running counter a la `gensym`. Assumes no other
   variable names use the prefix "var". (Otherwise, they might
   accidentally be the same as a generated variable name.) *)
let new_varname () : varid =
  gensym "var" ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let subst' = subst var_name repl in
  let double_sub v v' e = subst v (Var v') e |> subst' in 
  match exp with
  | Num _ | Bool _ | Float _ | Char _ | String _ | Raise | Unassigned -> exp
  | Var v -> if v  = var_name then repl else exp
  | Unop (u, e) ->  Unop (u, subst' e)
  | Binop (b, e1, e2) -> Binop (b, subst' e1, subst' e2)
  | App (e1, e2) -> App (subst' e1, subst' e2)
  | Conditional (e1, e2, e3) -> Conditional (subst' e1, subst' e2, subst' e3)
  | Fun (v, e) -> 
    if v = var_name then exp
    else if not (SS.mem v (free_vars repl)) then Fun (v, subst' e)
    else let v' = new_varname () in 
      Fun (v', double_sub v v' e)
  | Let (v, e1, e2) -> 
    if v = var_name then exp
    else if not (SS.mem v (free_vars repl)) then Let (v, subst' e1, subst' e2)
    else let v' = new_varname () in 
      Let (v', subst' e1, double_sub v v' e2)
  | Letrec (v, e1, e2) -> 
    if v = var_name then exp
    else if not (SS.mem v (free_vars repl)) then Letrec (v, subst' e1, subst' e2)
    else let v' = new_varname () in 
      Letrec (v', double_sub v v' e1, double_sub v v' e2)
 ;;
     
(*......................................................................
  String representations of expressions
 *)
 
(* Helpful infix for separating strings *)
let (^^^) a b = a ^ ", " ^ b ;;

let wrap s = "(" ^ s ^ ")" ;;
 
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  (* Local function renaming for clarity *)
  let etc = exp_to_concrete_string in

  (* Auxiliary functions to clean body, also allowing further implementation *)
  let match_unop (u : unop) : string = 
    match u with
    | Negate -> "~-"
    | Sin -> "sin"
    | Cos -> "cos"
    | Not -> "not"
  in

  let match_binop (b : binop) : string =
    match b with
    | Plus -> " + "
    | Minus -> " - "
    | Times -> " * "
    | Div -> " / "
    | Exp -> " ** "
    | Equals -> " = "
    | NotEquals -> " <> "
    | LessThan -> " < "
    | LessThanEq -> " <= "
    | GreaterThan -> " > "
    | GreaterThanEq -> " >= "
    | And -> " && "
    | Or -> " || "
    | Concat -> " ^ "
  in

  match exp with
  | Var x -> x
  | Num x -> string_of_int x
  | Float x -> string_of_float x
  | String x -> x
  | Char x -> String.make 1 x
  | Bool x -> string_of_bool x
  | Unop (o, e) -> (match_unop o) ^ (etc e)
  | Binop (b, e1, e2) -> (etc e1) ^ (match_binop b) ^ (etc e2)
  | Conditional (e1, e2, e3) -> 
    "if " ^ (etc e1) ^ " then " ^ (etc e2) ^ " else " ^ (etc e3)
  | Fun (v, e) -> "(fun " ^ v ^ " -> " ^ (etc e) ^ ")"
  | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ (etc e1) ^ " in " ^ (etc e2)
  | Letrec (v, e1, e2) -> "let rec " ^ v ^ " = " ^ (etc e1) ^ " in " ^ (etc e2)
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (e1, e2) -> (etc e1) ^ " " ^ (etc e2)
;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  (* Local renaming for clarity *)
  let eta = exp_to_abstract_string in

  (* Helper functions for clarity and for room in future augmentations *)
  let match_unop (u : unop) : string =
    match u with
    | Negate -> "Negate"
    | Sin -> "Sin"
    | Cos -> "Cos"
    | Not -> "Not"
  in

  let match_binop (b : binop) : string = 
    match b with
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Times -> "Times"
    | Div -> "Div"
    | Exp -> "Exp"
    | Equals -> "Equals"
    | NotEquals -> "NotEquals"
    | LessThan -> "LessThan"
    | LessThanEq -> "LessThanEq"
    | GreaterThan -> "GreaterThan"
    | GreaterThanEq -> "GreaterThanEq"
    | And -> "And"
    | Or -> "Or"
    | Concat -> "Concat"
  in

  match exp with
  | Var x -> "Var" ^ wrap x
  | Num x -> "Num" ^ wrap (string_of_int x)
  | Float x -> "Float" ^ wrap (string_of_float x)
  | Char x -> "Char" ^ wrap (String.make 1 x)
  | String x -> "String" ^ wrap x
  | Bool x -> "Bool" ^ wrap (string_of_bool x)
  | Unop (u, e) -> "Unop" ^ wrap((match_unop u) ^^^ (eta e))
  | Binop (b, e1, e2) -> 
    "Binop" ^ wrap ((match_binop b) ^^^ (eta e1) ^^^ (eta e2))
  | Conditional (e1, e2, e3) -> 
    "Conditional" ^ wrap ((eta e1) ^^^ (eta e2) ^^^ (eta e3))
  | Fun (v, e) -> "Fun" ^ wrap (v ^^^ (eta e))
  | Let (v, e1, e2) -> "Let" ^ wrap(v ^^^ (eta e1) ^^^ (eta e2))
  | Letrec (v, e1, e2) -> "Letrec" ^ wrap(v ^^^ (eta e1) ^^^ (eta e2))
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (e1, e2) -> "App" ^ wrap((eta e1) ^^^ (eta e2)) 
;;
