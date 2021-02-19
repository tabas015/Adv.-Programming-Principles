
(* LAB02 Done by 
		   Mulki Yusuf (yusuf204)
		  	   and 
		   Fahia Tabassum (tabas015)
DATE: 02/05/2021
*)

let num = fst ;;  
let den = snd ;;


let rec gcd i j =  
  if i <> 0  
  then if j > i  
       then gcd i (j - i)  
       else gcd (i - j) j  
  else j ;;

 
let  rat n d =
	let g = gcd n d
	in (n/g , d/g) ;;


let ratAdd a b =
	rat ( (num a * den b) + (den a * num b)) (den a * den b) ;;


let ratMul a b =
	rat (num a * num b) (den a * den b);;

let ratDiv a b =
	rat (num a * den b)(den a * num b);;


let ratGt a b =
	if (num a * den b) > (den a * num b)
	then true 
	else false ;;

let rec help c s t =
	if ratGt t (1 ,100000)
	then 
	let x = ratAdd s t in
	let y = ratDiv t c in
	let z = ratAdd c (1,1) in 
	help z x y
	else s;;

let rec euler() =
	let c = rat 1 1 in
	let s = rat 0 0 in
	let t = rat 1 1 in
	help c s t;;


(* 

TEST RESULTS ARE PROVIDED HERE

# #use "lab02.ml";;
val num : 'a * 'b -> 'a = <fun>
val den : 'a * 'b -> 'b = <fun>
val gcd : int -> int -> int = <fun>
val rat : int -> int -> int * int = <fun>
val ratAdd : int * int -> int * int -> int * int = <fun>
val ratMul : int * int -> int * int -> int * int = <fun>
val ratDiv : int * int -> int * int -> int * int = <fun>
val ratGt : int * int -> int * int -> bool = <fun>
val help : int * int -> int * int -> int * int -> int * int = <fun>
val euler : unit -> int * int = <fun>
# #use "tests2.ml";;
val ratPrint : int * int -> unit = <fun>
val boolPrint : bool -> unit = <fun>
1 / 2
- : unit = ()
1 / 2
- : unit = ()
1 / 1
- : unit = ()
5 / 6
- : unit = ()
5 / 1
- : unit = ()
8 / 15
- : unit = ()
1 / 10
- : unit = ()
3 / 2
- : unit = ()
true
- : unit = ()
false
- : unit = ()
Exception: Division_by_zero.
# 
*)