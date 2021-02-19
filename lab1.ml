
(* 
	Name: Fahia Tabassum 
	Id: tabas015
	Lab 01
	Date: 01/26/21 

*)

(* PROBLEM 01 *)

let rec howMany  e l =
	if l = []
	then 0
	
	else if e = List.hd l
		then 1 + howMany e (List.tl l ) 
		
		else 0 + howMany e (List.tl l) ;;


(* PROBLEM 02 *)

let rec delete  e l =
	if l = []
	then l

	else if e = List.hd l
		then  delete e (List.tl l) 

		else (List.hd l) :: delete e (List.tl l);;


(* PROBLEM 03 *)

let rec length a =
	if a = []
	then 0.0
	else 1. +. length (List.tl a) ;;
	

let rec mean l =
	
	if length l = 1.
	then List.hd l
	
	else ((List.hd l +. mean (List.tl l)) /. length l);;


(* 

RESULTS PRINTED OUT 

Fahias-MacBook-Air:lab 01 fahiatabassum$ OCaml
        OCaml version 4.11.1

# # use "lab1.ml";;
val howMany : 'a -> 'a list -> int = <fun>
val delete : 'a -> 'a list -> 'a list = <fun>
val length : 'a list -> float = <fun>
val mean : float list -> float = <fun>

# #use "tests1.ml" ;;
val printThings : ('a -> 'b, out_channel, unit) format -> 'a list -> unit =
  <fun>
0
- : unit = ()
1
- : unit = ()
1
- : unit = ()
0
- : unit = ()
2
- : unit = ()
0
- : unit = ()
[]
- : unit = ()
[]
- : unit = ()
[2 ; 3]
- : unit = ()
[1 ; 2 ; 3]
- : unit = ()
[2 ; 3 ; 4]
- : unit = ()
[x ; y]
- : unit = ()
1.000000
- : unit = ()
1.500000
- : unit = ()
0.250000
- : unit = ()
*)
