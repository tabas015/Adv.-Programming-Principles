(*    Project 01
        Done by Fahia Tabassum(tabas015)
*)

(* Helper function split, splits the list u in a two lists,
returns a tuple (l, r) with the left and right list, uses
l and r as temporary lists *)
let rec split u l r = 
    match u with 
    | [] | [_] -> (u @ l, r)
    | a::b::t -> split t (a :: l) (b::r)

(* Helper function combine, combines two lists given as
the tuple (l, r) in a single sorted list, uses s as a temporary list *)
let rec combine (l, r) s =
    match (l, r) with 
    | ([], _) | (_, []) -> s @ l @ r
    | (a::lt, b::rt) -> 
        if a < b then combine (lt, (b :: rt)) (s @ [a])
        else combine ((a::lt), rt) (s @ [b])

(* Function mergesort *)
let rec mergesort lst =
    (* Helper function applymsort, applies the mergesort function
    to the two lists in the tuple (l, r) *)
    let applymsort (l, r) = (mergesort l, mergesort r) in
    (* body of mergesort *)
    match lst with
    | [] | [_] -> lst   (* if zero or one elements, return list *)
    | ls -> combine (applymsort (split ls [] [])) []  (* if more than 2, split, recurse using split parts and combine results *)

(* Start function, tests the helper functions and mergesort *)
let () = 
    print_string "Testing: split [] [] [] = ([], [])... ";
    assert ((split [] [] []) = ([],[]));
    print_string "OK\n";
    print_string "Testing: split [1] [] [] = ([1], [])... ";
    assert ((split [1] [] []) = ([1],[]));
    print_string "OK\n";
    print_string "Testing: split [1;2] [] [] = ([1], [2])... ";
    assert ((split [1;2] [] []) = ([1],[2]));
    print_string "OK\n";
    print_string "Testing: split [1;2;3;4;5] [] [] = ([5;3;1], [4;2])... ";
    assert ((split [1;2;3;4;5] [] []) = ([5;3;1],[4;2]));
    print_string "OK\n";

    print_string "Testing: combine ([],[]) [] = []... ";
    assert ((combine ([], []) []) = []);
    print_string "OK\n";
    print_string "Testing: combine ([1],[]) [] = [1]... ";
    assert ((combine ([1], []) []) = [1]);
    print_string "OK\n";
    print_string "Testing: combine ([],[1]) [] = [1]... ";
    assert ((combine ([], [1]) []) = [1]);
    print_string "OK\n";
    print_string "Testing: combine ([2],[1]) [] = [1;2]... ";
    assert ((combine ([2], [1]) []) = [1;2]);
    print_string "OK\n";
    print_string "Testing: combine ([1;2;4],[3;5]) [] = [1;2;3;4;5]... ";
    assert ((combine ([1;2;4], [3;5]) []) = [1;2;3;4;5]);
    print_string "OK\n";

    print_string "Testing: mergesort [] = []... ";
    assert ((mergesort []) = []);
    print_string "OK\n";
    print_string "Testing: mergesort [1] = [1]... ";
    assert ((mergesort [1]) = [1]);
    print_string "OK\n";
    print_string "Testing: mergesort [3;2;1] = [1;2;3]... ";
    assert ((mergesort [3;2;1]) = [1;2;3]);
    print_string "OK\n";
    print_string "Testing: mergesort [7;6;3;6;3;1;7] = [1;3;3;6;6;7;7]... ";
    assert ((mergesort [7;6;3;6;3;1;7]) = [1;3;3;6;6;7;7]);
    print_string "OK\n";
    print_string "Testing: mergesort ['h';'b';'a';'g';'z';'l'] = ['a';'b';'g';'h';'l';'z']... ";
    assert ((mergesort ['h';'b';'a';'g';'z';'l']) = ['a';'b';'g';'h';'l';'z']);
    print_string "OK\n";
    print_string "Testing: mergesort [\"strings\";\"of\";\"a\";\"list\"] = [\"a\";\"list\";\"of\";\"strings\"]... ";
    assert ((mergesort ["strings";"of";"a";"list"]) = ["a";"list";"of";"strings"]);



(* TEST RESULTS PROVIDED HERE

# # use "mergesort.ml";;
val split : 'a list -> 'a list -> 'a list -> 'a list * 'a list = <fun>
val combine : 'a list * 'a list -> 'a list -> 'a list = <fun>
val mergesort : 'a list -> 'a list = <fun>
Testing: split [] [] [] = ([], [])... OK
Testing: split [1] [] [] = ([1], [])... OK
Testing: split [1;2] [] [] = ([1], [2])... OK
Testing: split [1;2;3;4;5] [] [] = ([5;3;1], [4;2])... OK
Testing: combine ([],[]) [] = []... OK
Testing: combine ([1],[]) [] = [1]... OK
Testing: combine ([],[1]) [] = [1]... OK
Testing: combine ([2],[1]) [] = [1;2]... OK
Testing: combine ([1;2;4],[3;5]) [] = [1;2;3;4;5]... OK
Testing: mergesort [] = []... OK
Testing: mergesort [1] = [1]... OK
Testing: mergesort [3;2;1] = [1;2;3]... OK
Testing: mergesort [7;6;3;6;3;1;7] = [1;3;3;6;6;7;7]... OK
Testing: mergesort ['h';'b';'a';'g';'z';'l'] = ['a';'b';'g';'h';'l';'z']... OK
Testing: mergesort ["strings";"of";"a";"list"] = ["a";"list";"of";"strings"]... OK
# 

*)