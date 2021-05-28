(*
   EVALUATE. An interpreter for a subset of Lisp.

     James Moen
     28 Mar 21

   Some things, like user functions, are missing. They will be added later.
*)

(* THING. The type of a Lisp object. *)

  type thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Primitive of (thing -> environment -> thing) |
    Symbol of string
  and
    environment = (string * thing) list ;;

(* EVALUEES. This tells what's visible outside the module EVALUATOR. *)

module type Evaluees =
sig

(* EVALUATE. A function that evaluates a Lisp expression. *)

  val evaluate: thing -> thing

end ;;

(* EVALUATOR. An evaluator for Lisp. *)

module Evaluator: Evaluees =
struct

(* EVALUATOR ERROR. Raised when the evaluator gets something bad. *)

  exception EvaluatorError of string ;;

(* OOPS. Raise EVALUATOR ERROR with MESSAGE. *)

  let oops message =
    raise (EvaluatorError message) ;;

(* Association list functions. *)

(* AL GET. Here ENVIRONMENT is an association list. Return the value associated
   with the string NAME in that list. Raise EVALUATOR ERROR if there is no such
   value. *)

  let alGet environment name =
    let rec alGetting environment =
      match environment
      with [] ->
             oops ("Unbound name: " ^ name) |
           (otherName, otherValue) :: otherEnvironment ->
             if name = otherName
             then otherValue
             else alGetting otherEnvironment
    in alGetting environment ;;

(* AL MAKE. Return a new empty association list. Duh. *)

  let alMake () =
    [] ;;

(* AL PUT. Add the string NAME and the Lisp object VALUE to the front of the
   association list ENVIRONMENT, and return the resulting association list. The
   new NAME and VALUE overrides the value of NAME if it was in ENVIRONMENT. *)

  let alPut environment name value =
    (name, value) :: environment ;;

(* Utility functions that do basic Lisp operations. It's simpler to write with
   these than to use MATCH rules. *)

(* CAR. Return the CAR of the list THINGS. *)

  let car things =
    match things
    with Cons (car, _) ->
          car |
         _ ->
          oops "CAR expected a CONS" ;;

(* CDR. Return the CDR of the list THINGS. *)

  let cdr things =
    match things
    with Cons (_, cdr) ->
          cdr |
         _ ->
          oops "CDR expected a CONS" ;;

(* IS CONS. Test if THING is a CONS. *)

  let isCons thing =
    match thing
    with Cons (_, _) ->
           true |
         _ ->
           false ;;

(* CONS. Return a list with CAR and CDR, where CDR is a list. *)

  let cons car cdr =
    if isCons cdr
    then Cons (car, cdr)
    else oops "CONS expected a CONS" ;;

(* GLOBAL. The global environment. It holds the bindings of Lisp symbols, which
   are represented as strings. *)

  let global = ref (alMake ()) ;;

(* TEE. The default TRUE value. NIL is acts like it's FALSE. Everything else
   (including TEE) acts like it's TRUE. *)

  let tee = Symbol "t" ;;

(* Add bindings of NIL and T to GLOBAL as special cases. *)

  global := alPut (! global) "nil" Nil ;;
  global := alPut (! global) "t"   tee ;;

(* EVALUATING. Evaluate the Lisp expression TERM and return the result. It does
   all the work for EVALUATE. EVALUATING applies CLOSUREs (user functions), it
   applies PRIMITIVEs (built-in functions), and it returns bindings of SYMBOLs.
   Everything else is returned unchanged. *)

  let rec evaluating term environment =
    match term
    with Cons (func, arguments) ->
           (match (evaluating func environment)
            with Closure (_, _, _) ->
                   oops "Not implemented yet" |
                 Primitive howTo ->
                   howTo arguments environment |
                 _ ->
                   oops "Function expected") |
         Symbol string ->
           alGet environment string |
         _ ->
           term ;;

(* Primitives. *)

(* PRIMITIVE. Define the string NAME to be a built-in function that's evaluated
   by calling HOW TO, with a list of its arguments and an environment in which
   to evaluate them. *)

  let primitive name howTo =
    global := alPut (! global) name (Primitive howTo) ;;

(* +. Add two integers. *)

  primitive "+"
   (fun arguments environment ->
     match arguments
     with Cons (left, Cons (right, Nil)) ->
           (match (evaluating left environment, evaluating right environment)
            with (Number left, Number right) ->
                   Number (left + right) |
                  _ ->
                    oops "+ expected two numbers") |
          _ ->
            oops "+ expected two numbers") ;;

(* AND. If there are no arguments then return TEE. Otherwise evaluate arguments
   left to right. If one is NIL then stop and return NIL. If none are NIL, then
   return the last argument. *)

  primitive "and"
   (fun arguments environment ->
     if arguments = Nil
     then tee
     else let rec anding arguments =
            match arguments
            with Cons (argument, Nil) ->
                  evaluating argument environment |
                 Cons (argument, otherArguments) ->
                  if (evaluating argument environment) = Nil
                  then Nil
                  else anding otherArguments |
                 _ ->
                  oops "AND expected zero or more things"
          in anding arguments) ;;

(* OR. If there are no arguments then return NIL. Otherwise evaluate arguments
   left to right. If one is not NIL then stop and return it. If none are not
   NIL, then return the last argument. *)

  primitive "or"
   (fun arguments environment ->
     if arguments = Nil
     then Nil
     else let rec oring arguments =
            match arguments
            with Cons (argument, Nil) ->
                  evaluating argument environment |
                 Cons (argument, otherArguments) ->
                  let argument = evaluating argument environment
                  in if argument = Nil
                     then oring otherArguments
                     else argument |
                 _ ->
                  oops "OR expected zero or more things"
          in oring arguments) ;;

(* CAR. Return the CAR of a CONS. *)

  primitive "car"
   (fun arguments environment ->
     match arguments
     with Cons (thing, _) ->
           car (evaluating thing environment) |
          _ ->
           oops "CAR expected a CONS") ;;

(* CONS. Return a list whose first element is CAR and whose remaining elements
   are in the list CDR. *)

  primitive "cons"
   (fun arguments environment ->
     match arguments
     with Cons (car, Cons (cdr, Nil)) ->
           cons
            (evaluating car environment)
            (evaluating cdr environment) |
          _ ->
           oops "CONS expected a THING and a CONS") ;;

(* IF. If the first argument is true, then evaluate the first argument. If it's
   false then evaluate the second argument instead. *)

  primitive "if"
   (fun arguments environment ->
     match arguments
     with Cons(test, Cons(ifTrue, Cons(ifFalse, Nil))) ->
           if (evaluating test environment) = Nil
           then evaluating ifFalse environment
           else evaluating ifTrue environment |
          _ ->
           oops "IF expected three THINGs") ;;

(* LIST. Return a list of zero or more arguments. *)

  primitive "list"
   (fun arguments environment ->
     let rec listing arguments =
      if arguments = Nil
      then Nil
      else cons
            (evaluating (car arguments) environment)
            (listing (cdr arguments))
     in listing arguments) ;;

(* QUOTE. Return the argument without evaluating it. *)

  primitive "quote"
   (fun arguments _ ->
     match arguments
     with Cons(thing, Nil) ->
           thing |
          _ ->
           oops "QUOTE expected a THING") ;;

(* EVALUATE. Users call this to evaluate the Lisp expression TERM. *)

  let evaluate term =
   evaluating term (! global) ;;

end ;;

(* Tests. *)

(* Evaluate (IF T (QUOTE YES) (QUOTE NO)). *)

Evaluator.evaluate
 (Cons (Symbol "if",
   Cons (Symbol "t",
    Cons (Cons (Symbol "quote", Cons (Symbol "yes", Nil)),
     Cons (Cons (Symbol "quote", Cons (Symbol "no", Nil)),
      Nil))))) ;;
