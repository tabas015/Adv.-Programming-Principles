

(* Lab 10
Done by Mulki Yusuf (yusuf204) and 
    Fahia Tabassum (tabas015) *)

type 
   thing =  
    Closure of thing * thing * environment |  
    Cons of thing * thing |  
    Nil |  
    Number of int |  
    Primitive of (thing -> environment -> thing) |  
    Symbol of string  
  and  
    environment = (string * thing) list ;;


open Printf;;
exception BadThing ;;



let rec printingThing thing =
    match thing with
        Closure(_, _, _) -> printf "[Closure]" |
        Cons(car, cdr) -> 
            printf ("(") ; printingThing car ; printingThings cdr ; printf (")")|
        Nil -> printf("nil") |
        Number integer -> printf "%i" integer  |
        Primitive _ -> printf "[Primitive]" |
        Symbol string -> printf "%s" string 
            

and printingThings things = 
    match things 
    with
    Cons (a,b) -> printf " " ; printingThing a ; printingThings b |
    Nil -> () |
    _ -> raise BadThing
;;


let printThing thing = 
    printingThing thing ; printf  "\n"

;;
