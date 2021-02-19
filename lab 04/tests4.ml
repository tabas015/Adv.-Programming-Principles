(*
  CSci 2041 Lab Assignment 4

    James Moen
    16 Feb 21

  It's worth 30 points.
*)

(*

  PUT YOUR CODE HERE !!!

*)

(* PRINT THINGS. Print a list of THINGS using a FORMAT string. *)

let printThings format things =
  let rec printingThings things =
    match things
    with [] -> () |
         firstThing :: otherThings ->
           Printf.printf " ; " ;
           Printf.printf format firstThing ;
           printingThings otherThings
  in Printf.printf "[" ;
     (match things
      with [] -> () |
           firstThing :: otherThings -> 
             Printf.printf format firstThing ;
             printingThings otherThings) ;
     Printf.printf "]\n" ;;

(* Tests. Each test is worth some number of points. If your functions produce
   the results that the test expects, then you get the points. Your score for
   this assignment will be the total number of points in all the tests. *)

printThings "%i" (allbut [] 0) ;;            (* 1 pt [] *)

printThings "%i" (allbut [0; 1; 2] 0) ;;     (* 1 pt [1; 2] *)

printThings "%i" (allbut [0; 1; 2] 1) ;;     (* 1 pt [0; 2] *)

printThings "%i" (allbut [0; 1; 2] 2) ;;     (* 1 pt [0; 1] *)

printThings "%i" (allbut [0; 1; 2] 7734) ;;  (* 1 pt [0; 1; 2] *)

(* In the following tests, it doesn't matter what CHOOSE returns. All we care
   about is what the tests print. *)

choose (fun thing -> Printf.printf "%i" thing) [] ;;

(* 1 pt. if it prints nothing. *)

choose (fun thing -> Printf.printf "%i " thing) [1] ;;

(* 1 pt. if it prints: 1 *)

choose (fun thing -> Printf.printf "%i " thing) [0; 1; 2] ;;

(* 3 pts. if it prints: 0 1 2. *)

(* In the following tests, it also doesn't matter what PERMUTE returns. All we
   care about is what the tests print. *)

permute
 (fun things -> printThings "%i" things)
 [] ;;

(* 5 pts. if it prints nothing. *)

permute
 (fun things -> printThings "%i" things)
 [0] ;;

(* 5 pts. if it prints [0]. *)

permute
 (fun things -> printThings "%i" things)
 [0; 1; 2] ;;

(* 10 pts. if it prints this:

[2 ; 1 ; 0]
[1 ; 2 ; 0]
[2 ; 0 ; 1]
[0 ; 2 ; 1]
[1 ; 0 ; 2]
[0 ; 1 ; 2]

  Your lists might appear in a different order, but each of the six lists must
  be printed exactly once, and no list must be printed more than once. *)
