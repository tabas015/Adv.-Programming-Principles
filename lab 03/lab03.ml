(* LAB03 Done by 
		   Mulki Yusuf (yusuf204)
		  	   and 
		   Fahia Tabassum (tabas015)
DATE: 02/11/2021
*)


exception BadEmptyBst;; 
type 'key bst = BstEmpty | BstNode of 'key * 'key bst * 'key bst ;;

let rec bstMaxKey tree =  
  match tree  
  with BstEmpty -> raise BadEmptyBst |  
       BstNode (k, _, BstEmpty)   -> k |  
       BstNode (_, _, r) -> bstMaxKey r;;

let rec bstDelete tree key =

	let rec bstDeleting subtree =
		(match subtree 
		with  
		BstEmpty -> BstEmpty 
	
		| BstNode(k, BstEmpty, BstEmpty) -> 
			if key = k
			then BstEmpty
			else subtree 
		| BstNode (k,BstEmpty, r) -> 
			if key = k
			then r
			else 
				if key > k 
				then BstNode (k, BstEmpty, bstDeleting r)
				else BstNode (k, BstEmpty, r)

		| BstNode (k,l,BstEmpty) -> 
		if key = k
			then l
			else 
				if key < k 
				then BstNode (k,bstDeleting l, BstEmpty)
				else BstNode (k, l, BstEmpty)
		
		| BstNode(k,l, r) -> 
			if key < k 
			then BstNode (k, bstDeleting l, r)
			else 
				if key > k
				then BstNode (k,l, bstDeleting r)
				else 
				BstNode(bstMaxKey l, bstDelete l (bstMaxKey l), r))

	in bstDeleting tree ;;

(* TEST RESULTS 


val bstInsert : 'a bst -> 'a -> 'a bst = <fun>
val bstIsIn : 'a -> 'a bst -> bool = <fun>
val t : 'a bst = BstEmpty
val t : int bst = BstNode (100, BstEmpty, BstEmpty)
val t : int bst = BstNode (100, BstNode (70, BstEmpty, BstEmpty), BstEmpty)
val t : int bst =
  BstNode (100, BstNode (70, BstEmpty, BstEmpty),
   BstNode (137, BstEmpty, BstEmpty))
val t : int bst =
  BstNode (100, BstNode (70, BstNode (53, BstEmpty, BstEmpty), BstEmpty),
   BstNode (137, BstEmpty, BstEmpty))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstEmpty, BstEmpty)),
   BstNode (137, BstEmpty, BstEmpty))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty, BstEmpty))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty,
    BstNode (212, BstNode (149, BstEmpty, BstEmpty), BstEmpty)))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty,
    BstNode (212, BstNode (149, BstEmpty, BstEmpty),
     BstNode (997, BstEmpty, BstEmpty))))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty,
    BstNode (212, BstEmpty, BstNode (997, BstEmpty, BstEmpty))))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (74, BstEmpty, BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
val t : int bst =
  BstNode (74, BstNode (70, BstNode (53, BstEmpty, BstEmpty), BstEmpty),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
val t : int bst =
  BstNode (74, BstNode (53, BstEmpty, BstEmpty),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
val t : int bst =
  BstNode (74, BstNode (53, BstEmpty, BstEmpty),
   BstNode (212, BstEmpty, BstEmpty))
val t : int bst = BstNode (74, BstEmpty, BstNode (212, BstEmpty, BstEmpty))
val t : int bst = BstNode (74, BstEmpty, BstEmpty)
val t : int bst = BstEmpty

*)
