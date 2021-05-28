(* CSCI Lab 12 Done by 
           Mulki Yusuf (yusuf204)
               and 
           Fahia Tabassum (tabas015)

*)

(*
  Tests for CSci 2041 Lab 12
*)

(* MUTY BST. A mutable binary search tree that associates KEYs with VALUEs. *)

type ('key, 'value) mutyBst =
  Empty |                            (* An empty BST. *)
  Node of
    'key *                           (* The key, duh. *)
    'value ref *                     (* The key's value. *)
    ('key, 'value) mutyBst ref *     (* Left subtree with keys < key. *)
    ('key, 'value) mutyBst ref ;;    (* Right subtree with keys > key. *)

(* MUTY BST ERROR. Raised if a BST method detects an error. *)

exception MutyBstError of string ;;


let makeMutyBstNode key value = 
  Node (key,ref value, ref Empty,ref Empty);;


let makeMutyBst () = 
  object 
  val root = ref Empty

  method get key =
    let rec getting subtree= 
    match subtree
    with Empty -> raise (MutyBstError "Error") |
        Node (otherKey,otherValue,left,right) -> 
          if otherKey < key
          then getting !right
          else if otherKey > key
          then  getting !left
          else !otherValue
    in getting !root

 
  method isEmpty () =
    if !root == Empty
    then true 
    else false
 
    
  method put key value =
    let rec putting subtree =
    match subtree
    with Empty -> root := makeMutyBstNode key value |
        Node (otherKey,otherValue,left,right) -> 
        if otherKey > key
          then 
            if !left == Empty
            then  left :=  (makeMutyBstNode key value)
            else  putting !left
        
        else if otherKey < key
          then
            if !right == Empty
            then right :=  (makeMutyBstNode key value)
            else   putting !right

        else putting (makeMutyBstNode key !otherValue)

    in  putting !root

    method height () = 
      let count = 0 
      in 
      let rec heighting subtree = 
        match subtree 
        with Empty -> count |
          Node (otherKey,otherValue,left,right) ->
          if !left > !right
          then (heighting !left)  + 1  
          else (heighting !right)  + 1

    in heighting !root

 

end ;;



(* Tests, worth 30 pt. The calls to PUT should create a balanced BST. *)

makeMutyBstNode "one" 1 ;;

(* Node ("one",
         {contents = 1},
         {contents = Empty},
         {contents = Empty})                                           2 pt. *)

let bst = makeMutyBst () ;;

(* val bst :
     < get : '_a -> '_b;
       height : unit -> int;
       isEmpty : unit -> bool;
       put : '_a -> '_b -> unit > = <obj>                              5 pt. *)

bst#height () ;;                                                 (* 0  2 pt. *)

bst#isEmpty () ;;                                             (* true  2 pt. *)

bst#put 4 "four" ;;                                             (* ()  1 pt. *)

bst#put 2 "two" ;;                                              (* ()  1 pt. *)

bst#put 1 "one" ;;                                              (* ()  1 pt. *)

bst#put 3 "three" ;;                                            (* ()  1 pt. *)

bst#put 6 "six" ;;                                              (* ()  1 pt. *)

bst#put 5 "five" ;;                                             (* ()  1 pt. *)

bst#put 7 "seven" ;;                                            (* ()  1 pt. *)

bst#height () ;;                                                 (* 3  5 pt. *)

bst#get 1 ;;                                                 (* "one"  1 pt. *)

bst#get 2 ;;                                                 (* "two"  1 pt. *)

bst#get 3 ;;                                               (* "three"  1 pt. *)

bst#get 4 ;;                                                (* "four"  1 pt. *)

bst#get 5 ;;                                                (* "five"  1 pt. *)

bst#get 6 ;;                                                 (* "six"  1 pt. *)

bst#get 7 ;;                                               (* "seven"  1 pt. *)

(* TEST RESULTS
type ('key, 'value) mutyBst =
    Empty
  | Node of 'key * 'value ref * ('key, 'value) mutyBst ref *
      ('key, 'value) mutyBst ref
exception MutyBstError of string
val makeMutyBstNode : 'a -> 'b -> ('a, 'b) mutyBst = <fun>
val makeMutyBst :
  unit ->
  < get : 'a -> 'b; height : unit -> int; isEmpty : unit -> bool;
    put : 'a -> 'b -> unit > =
  <fun>
- : (string, int) mutyBst =
Node ("one", {contents = 1}, {contents = Empty}, {contents = Empty})
val bst :
  < get : '_weak48 -> '_weak49; height : unit -> int; isEmpty : unit -> bool;
    put : '_weak48 -> '_weak49 -> unit > =
  <obj>
- : int = 0
- : bool = true
- : unit = ()
- : unit = ()
- : unit = ()
- : unit = ()
- : unit = ()
- : unit = ()
- : unit = ()
- : int = 3
- : string = "one"
- : string = "two"
- : string = "three"
- : string = "four"
- : string = "five"
- : string = "six"
- : string = "seven"
*)