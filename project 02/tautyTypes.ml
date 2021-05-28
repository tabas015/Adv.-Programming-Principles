
(* PROJECT 02
    Done by 
    Fahia Tabassum(tabas015) 
*)

(* PROPOSITION. Represent an expression in propositional logic. *)

type proposition =
  False |
  True |
  Var of string |
  And of proposition * proposition |
  Or of proposition * proposition |
  Not of proposition |
  Imply of proposition * proposition |
  Equiv of proposition * proposition ;;

(* CONDITIONAL. Represent an IF term. *)

type conditional =
  IffyFalse |
  IffyTrue |
  IffyVar of string |
  If of conditional * conditional * conditional ;;

 
let ifify p =
  let rec ififying prop =
  match prop 
  
  with False               -> IffyFalse |
       True                -> IffyTrue |
       Var name            -> IffyVar name |
       Not right           -> If (ififying right, IffyFalse, IffyTrue) |
       And (left, right)   -> If (ififying left, ififying right, IffyFalse) |
       Or (left, right)    -> If (ififying left ,IffyTrue,ififying right) |
       Imply (left, right) -> If (ififying left,ififying right, IffyTrue)  |
       Equiv (left, right) -> let r = ififying right 
                              in 
                              (If (ififying left ,r , If (r ,IffyFalse ,IffyTrue)))
  
  in ififying p ;;
 
let rec normalize c = 
  let rec normalizing  l alpha2 beta2 =
    match l
    with 
    If (pi, alpha1, beta1) -> normalizing pi (If (alpha1, alpha2, beta2)) (If(beta1, alpha2, beta2)) |
    _ -> If ( l , normalize alpha2 , normalize beta2 )

  in 
  match c
  with 
  If (otherPi, otherAlpha1, otherBeta1)  -> normalizing otherPi otherAlpha1 otherBeta1 |
  _  ->  c ;;
 

let substitute c v b = 
  let rec substituting c  =
    match c
    with 
    If (pi, alpha, beta ) ->  If (substituting pi , substituting alpha , substituting beta )  | 
    _ -> 
    
    if c = v
    then b 
    else c

  in substituting c ;;

let simplify c = 
  let rec simplifying cond =
    match cond 
    with
    If (IffyTrue, alpha, beta) -> simplifying alpha (* RULE 7 *) |
    If (IffyFalse, alpha, beta) -> simplifying beta (* RULE 8 *) |
    If (pi, alpha, beta ) -> 
      let a' = (simplifying (substitute alpha pi IffyTrue) )
      in
      let b' = (simplifying (substitute beta pi IffyFalse)) 
      in
      if pi = IffyTrue          
      then a' 
      else if  b' = IffyFalse    
      then b'
      
      else if a' = b'
      then simplifying alpha              (* RULE 10*)
      
      else 
      if a' = IffyTrue && b' = IffyFalse
      then pi                             (* RULE 09*)
      else If ( pi, a', b')               (* RULE 11*)

    
    |
    _ -> cond
  

  in simplifying c;;

  
let tautology p = 
  (simplify (normalize (ifify p))) = IffyTrue ;;



(* Q1. A test case. This is ¬ (α ∧ β) → (¬ α ∨ ¬ β). It's a tautology. *)

let q1 =
  (Imply
    (Not
      (And
        (Var "p", Var "q")),
     Or
      (Not
        (Var "p"),
       Not
        (Var "q")))) ;;

(* Q2. A test case. This is α ∧ β. It's not a tautology.  *)

let q2 =
  (And
    (Var "p", Var "q")) ;;


(*

ADDITIONAL TEST CASES
Q3. A test case. This (p → q) ∧ (q → p). It's  a tautology.  

*)


let q3 =
  (Or
    (Imply(
      Var "p", Var "q") ,

    Imply(
      Var "q", Var "p")));;


(*Q4. A test case. This (p → q) ∧ (q → r). It's  not a tautology.  *)

let q4 =
  (And
    (Imply(
      Var "p", Var "q") ,

    Imply(
      Var "q", Var "r")));;

(*Q5. A test case. This (p → q) ∨ (q → p). It's  a tautology.  *)

let q5 =
  (Or
    (Imply(
      Var "p", Var "q") ,

    Imply(
      Var "q", Var "p")));;

(*Q5. A test case. This ¬ (p → q) ∨ (q → p). It's not a tautology.  *)

let q6 =
  (Or
    (Not 
      (Imply(
        Var "p", Var "q")) ,

    Imply(
      Var "q", Var "p")));;


(* Beware: Q1 and Q2 are not exhaustive tests! Your code is not necessarily
   correct if it works on them. *)





(* TEST RESULTS 


# # use "tautyTypes.ml";;
type proposition =
    False
  | True
  | Var of string
  | And of proposition * proposition
  | Or of proposition * proposition
  | Not of proposition
  | Imply of proposition * proposition
  | Equiv of proposition * proposition
type conditional =
    IffyFalse
  | IffyTrue
  | IffyVar of string
  | If of conditional * conditional * conditional
val ifify : proposition -> conditional = <fun>
val normalize : conditional -> conditional = <fun>
val substitute : conditional -> conditional -> conditional -> conditional =
  <fun>
val simplify : conditional -> conditional = <fun>
val tautology : proposition -> bool = <fun>
val q1 : proposition =
  Imply (Not (And (Var "p", Var "q")), Or (Not (Var "p"), Not (Var "q")))
val q2 : proposition = And (Var "p", Var "q")
val q3 : proposition =
  Or (Imply (Var "p", Var "q"), Imply (Var "q", Var "p"))
val q4 : proposition =
  And (Imply (Var "p", Var "q"), Imply (Var "q", Var "r"))
val q5 : proposition =
  Or (Imply (Var "p", Var "q"), Imply (Var "q", Var "p"))
val q6 : proposition =
  Or (Not (Imply (Var "p", Var "q")), Imply (Var "q", Var "p"))



# tautology q1;;
- : bool = true
# tautology q2;;
- : bool = false
# tautology q3;;
- : bool = true
# tautology q4;;
- : bool = false
# tautology q5;;
- : bool = true
# tautology q6;;
- : bool = false




*)
