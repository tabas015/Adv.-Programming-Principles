(* Csci Lab 7 done by Mulki Yusuf (yusuf204)
               and 
           Fahia Tabassum (tabas015)
  


  Tests for CSci 2041 Lab 7
  45 points.
*)

(* HASH. Return the index of a bucket in TABLE where KEY may be found. TABLE is
   made by calling HASH MAKE. *)

let hash table key =
  abs ((Hashtbl.hash key) mod (Array.length table)) ;;

(* HASH MAKE. Return an array to be used as a hash table. It contains MODULUS
   buckets. We assume MODULUS is a prime number, far from a power of 2. *)

let hashMake modulus =
  Array.make modulus NoPair ;;


(*

  YOUR CODE GOES HERE!

*)

(* Tests, worth 45 points. Note that the table size is small enough that you
   can see the entire table by typing "table ;;" to the OCaml REPL. *)

let table = hashMake 23 ;;                        (* [| NoPair ... |] 0 pt. *)

hashHas table "uno" ;;                            (* false            3 pt. *)

hashPut table "uno" "one" ;;                     (* ()               3 pt. *)

hashHas table "uno" ;;                            (* true             3 pt. *)

hashGet table "uno" ;;                            (* "one"            3 pt. *)

hashPut table "duo" "two" ;;                     (* ()               3 pt. *)

hashPut table "trio" "three" ;;                  (* ()               3 pt. *)

hashHas table "trio" ;;                           (* true             3 pt. *)

hashDelete table "duo" ;;                         (* ()               3 pt. *)

hashHas table "duo" ;;                            (* false            3 pt. *)

hashPut table "duo" "bleen" ;;                   (* ()               3 pt. *)

hashGet table "duo" ;;                            (* "bleen"          3 pt. *)

hashGet table "trio" ;;                           (* "three"          3 pt. *)

hashHas table "bleen" ;;                          (* false            3 pt. *)

hashDelete table "bleen" ;;                       (* ()               3 pt. *)

hashGet table "uno" ;;                            (* "one"            3 pt. *)
