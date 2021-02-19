(* LAB04 Done by 
		   Mulki Yusuf (yusuf204)
		  	   and 
		   Fahia Tabassum (tabas015)
DATE: 02/19/2021
*)

let choose etc things =
	let rec choosing things=  
		match things 
		with
		[] -> () |
		firstThing :: otherThings -> 
			etc firstThing ; choosing otherThings
	in choosing things;;
	 

let rec allbut things thing = 
	match things 
	with [] -> things |
	firstThing :: otherThings -> 	
		if thing = firstThing 
		then otherThings  
		else  firstThing :: allbut otherThings thing ;;


let permute etc things =
	let rec permuting permutedThings unpermutedThings =
		match unpermutedThings 
		with 
		[] -> etc permutedThings |
		_ ->
    	choose (fun unpermutedthing -> permuting (unpermutedthing :: permutedThings) (allbut unpermutedThings unpermutedthing)) unpermutedThings 
	in permuting [] things ;;
	
(** 
TEST RESULTS:

val printThings : ('a -> 'b, out_channel, unit) format -> 'a list -> unit =
  <fun>
[]
- : unit = ()
[1 ; 2]
- : unit = ()
[0 ; 2]
- : unit = ()
[0 ; 1]
- : unit = ()
[0 ; 1 ; 2]
- : unit = ()
- : unit = ()
1 - : unit = ()
0 1 2 - : unit = ()
[]
- : unit = ()
[0]
- : unit = ()
[2 ; 1 ; 0]
[1 ; 2 ; 0]
[2 ; 0 ; 1]
[0 ; 2 ; 1]
[1 ; 0 ; 2]
[0 ; 1 ; 2]
- : unit = ()

**)
