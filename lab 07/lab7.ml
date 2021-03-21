

(* Csci Lab 7 
done by Mulki Yusuf (yusuf204)
               and 
           Fahia Tabassum (tabas015)
  

*)



exception NoSuchKey;;

  type ('key, 'value) pair = 
    NoPair |
      Pair of 'key * 
            'value ref * 
            ('key, 'value) pair ref ;;

  let hashMake modulus =  
  Array.make modulus NoPair ;;

  let hash table key =  
  abs ((Hashtbl.hash key) mod (Array.length table)) ;;


  let hashDelete table key = 
    let newpairs = hash table key in 
      let rec hashDeleting pairs =
        match pairs 
        with
        NoPair -> pairs |
        Pair (hashkey, hashval, hashlist) -> 
            if key = hashkey
            then !hashlist
      else 
      ((hashlist := hashDeleting (!hashlist);
        pairs))
    in table.(newpairs) <- hashDeleting table.(newpairs);; 


  

  let hashGet table key =
    let newpairs = hash table key in 
    
    let rec hashGetting pairs =
    match pairs 
    with NoPair -> raise NoSuchKey |
        Pair (otherKey, otherValue, otherPairs) ->
            if key = otherKey
            then !otherValue
            else (hashGetting (!otherPairs))
    in hashGetting table.(newpairs) ;;

  

  let hashHas table key=
    (*let pairs = hash table key in *)
    
    let rec hashhasing pairs =
    match pairs 
    with NoPair -> false |
        Pair(otherKey, otherValue, otherPairs) ->
            if key = otherKey
            then true
            else hashhasing (!otherPairs)

    in hashhasing table.(hash table key);;

  

  let hashPut table key value = 
    let index = hash table key in 
    let rec hashPutting pairs = 
    match pairs 
    with NoPair ->  
    table.(index) <- Pair(key, ref value, ref table.(index)) |
    Pair(otherKey, otherValue, otherPairs) ->
            if key = otherKey
            then otherValue := value
            else hashPutting (!otherPairs)  
    
    in hashPutting table.(index)
  ;;


  (* Test Results: 
  val hash : 'a array -> 'b -> int = <fun>
  val hashMake : int -> ('a, 'b) pair array = <fun>
  val table : ('_weak17, '_weak18) pair array =
  [|NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair;
    NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair;
    NoPair; NoPair; NoPair; NoPair; NoPair|]
- : bool = false
- : unit = ()
- : bool = true
- : string = "one"
- : unit = ()
- : unit = ()
- : bool = true
- : unit = ()
- : bool = false
- : unit = ()
- : string = "bleen"
- : string = "three"
- : bool = false
- : unit = ()
- : string = "one"
  
  *)