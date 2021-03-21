
(*Lab 06 
Done by - Mulki Yusuf (yusuf204)
		  	   and 
		   Fahia Tabassum (tabas015)*)

(* NODE TYPE with a base element AND left pointer and right pointer*)

type 'base mutyQueue =  
  MutyQueueNode  
  of 'base * 
     'base mutyQueue ref * 
     'base mutyQueue ref ;;

let mutyQueueMake s =
	let rec h = MutyQueueNode (s, ref h,  ref h)  
    in h


let mutyQueueEmpty q =
	match q 
	with
	MutyQueueNode ( _,  l, r) -> 
		if !l ==  q && !r == q
		then true
		else false ;;
		

let mutyQueueEnqueue q e = 
	match q 
	with 
	MutyQueueNode ( s , l , _ ) -> 
		let newNode = MutyQueueNode (e, ref !l, ref q) in
				
	match !l 
	with MutyQueueNode (ele, left, queue) ->
	l := newNode;
	queue := newNode;
	;;
		
let  mutyQueueDequeue q =
	match q 
	with 
	MutyQueueNode ( _, l , r )  -> 
	
	match !r 
	with 
	MutyQueueNode ( deletedNode , right , queue ) ->
	 
	match !queue
	with MutyQueueNode ( newdeletedNode , newright , newqueue ) ->
	newright := q;
	r := !queue;
	deletedNode
	;;

	(* TEST RESULTS:
	type 'base mutyQueue =
    MutyQueueNode of 'base * 'base mutyQueue ref * 'base mutyQueue ref
val queue : string mutyQueue/2 =
  MutyQueueNode ("", {contents = <cycle>}, {contents = <cycle>})
- : bool = true
- : string = ""
- : unit = ()
- : bool = false
- : unit = ()
- : unit = ()
- : string = "A"
- : string = "B"
- : string = "C"
- : bool = true
- : string = ""
- : string = ""
 *)


