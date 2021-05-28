


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
 

let normalize c = (* so for l I have checked a test and two IFs . should I need to have any other cases?*)
	let rec normalizing  l alpha2 beta2 =
 		match l
 		with 
 		If (pi, alpha1, beta1) -> normalizing pi (If (alpha1, alpha2, beta2)) (If(beta1, alpha2, beta2)) |
 		_ -> l 

 	in 
 	match c
 	with 
 	If (otherPi, otherAlpha1, otherBeta1)  -> normalizing otherPi otherAlpha1 otherBeta1 |
 	_  ->  c ;;
 

let simplify c = 
 	let rec simplifying cond =
 		match cond 
 		with
 		If (IffyTrue, alpha, beta) -> simplifying alpha |
 		If (IffyFalse, alpha, beta) -> simplifying beta |
 		If (pi, IffyTrue, IffyFalse) -> simplifying pi  |
 		If (pi, alpha, beta ) -> 
 			if simplifying alpha = simplifying beta 
 			then simplifying alpha 
 			else  If (pi , (substitute alpha pi IffyTrue) , (substitute beta pi IffyFalse) )
 	in simplifying c;;


let substitute c v b = 
 	let rec substituting c v b =
 		match c
 		with 
 		If (pi, alpha, beta ) ->  If  ( substituting pi, substituting alpha , substituting beta ) |
 		 _ -> 
 		 if 
 		 pi = alpha 
 		 then IffyTrue
 		else IffyFalse

 		 
 	in substituting c v b ;;

 
(*let tautology p = 
	ifify (p) *)


