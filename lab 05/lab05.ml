
(*Lab 05 
Done by - Mulki Yusuf (yusuf204)
		  	   and 
		   Fahia Tabassum (tabas015)*)



let makeStream this state next =  
  ((this, state), next) ;;  
  
let first ((this, state), next) =  
  this ;;  
  
let rest ((this, state), next) =  
  (next this state, next) ;;

let odds =
	makeStream 1 2 (fun this state -> (this + state , state )) ;;

let rec take count stream =  
  match count  
  with 0 -> [] |  
       _ -> (first stream) :: take (count-1) (rest stream) ;;

let rec trim count stream =
 match count  
  with 0 -> stream |  
       _ ->   trim (count-1) (rest stream) ;;

	
let rec scale factor stream =	
    makeStream (first stream * factor) (rest stream) (fun this state -> (first state * factor),(rest state))
;;

let rec sum left right =
  makeStream (first left + first right) (rest left, rest right) 
  (fun this state -> match state with newleft, newright -> ((first newright  + first newleft), (rest newleft, rest newright)))
;;
        
(*TEST RESULTS:

val makeStream : 'a -> 'b -> 'c -> ('a * 'b) * 'c = <fun>
val first : ('a * 'b) * 'c -> 'a = <fun>
val rest : ('a * 'b) * ('a -> 'b -> 'c) -> 'c * ('a -> 'b -> 'c) = <fun>
val take : int -> ('a * 'b) * ('a -> 'b -> 'a * 'b) -> 'a list = <fun>
val naturals : (int * unit) * (int -> '_weak47 -> int * unit) =
  ((0, ()), <fun>)
- : int = 1
- : int = 3
- : int = 5
- : int list = [1; 3; 5; 7; 9; 11; 13]
val but1st5 : (int * unit) * (int -> unit -> int * unit) = ((5, ()), <fun>)
- : int = 5
- : int = 6
- : int = 7
- : int list = [5; 6; 7; 8; 9; 10; 11]
val byFives :
  (int * ((int * unit) * (int -> unit -> int * unit))) *
  ('_weak48 ->
   (int * '_weak49) * (int -> '_weak49 -> '_weak50) ->
   int * ('_weak50 * (int -> '_weak49 -> '_weak50))) =
  ((0, ((1, ()), <fun>)), <fun>)
- : int = 0
- : int = 5
- : int = 10
- : int list = [0; 5; 10; 15; 20; 25; 30]
val natsPlusByFives :
  (int *
   (((int * unit) * (int -> unit -> int * unit)) *
    ((int * ((int * unit) * (int -> unit -> int * unit))) *
     (int ->
      (int * unit) * (int -> unit -> int * unit) ->
      int * ((int * unit) * (int -> unit -> int * unit)))))) *
  ('_weak51 ->
   ((int * '_weak52) * (int -> '_weak52 -> '_weak53)) *
   ((int * '_weak54) * (int -> '_weak54 -> '_weak55)) ->
   int *
   (('_weak53 * (int -> '_weak52 -> '_weak53)) *
    ('_weak55 * (int -> '_weak54 -> '_weak55)))) =
  ((0, (((1, ()), <fun>), ((5, ((2, ()), <fun>)), <fun>))), <fun>)
- : int = 0
- : int = 6
- : int list = [0; 6; 12; 18; 24; 30; 36]

*)