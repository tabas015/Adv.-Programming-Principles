
(* 
Lab 11
Done by Mulki Yusuf (yusuf204) and 
    Fahia Tabassum (tabas015) *)



(*
   TESTS 11. The Lisp interpreter LISP.ML, modified with tests for Lab 11.

     James Moen
     13 Apr 21

   This is worth 40 points.
*)

(* THING. A Lisp object.

   ∙ ENVIRONMENT is an association list that maps STRINGs from SYMBOLs to the
     THINGs that are the SYMBOLs' values.

   ∙ CLOSURE (PARS, BODY, ENV) is a user function created by LAMBDA (or λ).
     PARS is a Lisp list of zero or more distinct SYMBOLs, the function's
     parameters. BODY is a Lisp expression that is the function's body. ENV
     is an ENVIRONMENT that records all SYMBOL bindings when the CLOSURE was
     created. It's used to evaluate BODY.

   ∙ CONS builds Lisp lists. CONS (A, D) means the cons cell whose CAR is A and
     whose CDR is D. So the Lisp list (E₁ E₂ ... En) is represented like this:
     CONS (E₁, CONS (E₂ ..., CONS (En, NIL) ... )).

   ∙ NIL is the empty Lisp list. It also means FALSE. Any Lisp object other
     than NIL means TRUE. The lisp symbol T is sometimes used to mean TRUE.

   ∙ NUMBER K is the integer constant K.

   ∙ PRIMITIVE H is a Lisp function that's built into the evaluator. H is an
     OCaml function (FUN ARGS ENV -> BODY) that takes a Lisp list of arguments
     ARGS and an environment ENV. It returns the result of the OCaml expression
     BODY.

   ∙ SYMBOL S is a symbol whose characters are in the string S. *)


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

(* EVALUATORS. What's visible outside the module EVALUATOR. *)

module type Evaluators =
sig
  val evaluate: thing -> thing
end ;;

(* EVALUATOR. The Lisp evaluator. *)

module Evaluator: Evaluators =
struct

(* OOPS. Call this with a descriptive error MESSAGE in case of error. *)

  exception EvaluatorError of string ;;
  

  let oops message =
    raise (EvaluatorError message) ;;

(* ENV GET. Search an environment ENV for the value of a symbol whose string is
   NAME, and return that value. If we can't find it, then call the continuation
   ETC. *)

  let envGet env name etc =
    let rec envGetting env =
      match env
      with [] ->
             etc () |
           (otherName, otherValue) :: otherEnv ->
             if name = otherName
             then otherValue
             else envGetting otherEnv
    in envGetting env ;;

(* ENV MAKE. Return a new empty environment. *)

  let envMake () =
    [] ;;

(* ENV PUT. Return a new environment like ENV, but the symbol with the string
   NAME is bound to VALUE. *)

  let envPut name value env =
    (name, value) :: env ;;

(* TEE. The Lisp symbol T. It means TRUE sometimes. *)

  let tee = Symbol "t" ;;

(* GLOBAL. The global environment. It's a variable, so DEFINE (see below) can
   change it. Initially it binds Lisp symbols NIL and T. *)

  let global = ref (envMake ()) ;;

  global := envPut "nil" Nil (! global) ;;
  global := envPut "t"   tee (! global) ;;

(* LOOKUP. Return the value of a symbol whose string is NAME. First search the
   local environment ENV. If we can't find NAME there, then search GLOBAL, the
   global environment. It's an error if we can't find NAME there either. *)

  let lookup env name =
    envGet env name
      (fun () ->
         envGet (! global) name
           (fun () ->
             oops ("Unbound name " ^ name))) ;;

(* EVALUATING. Do all the work for EVALUATE. *)

  let rec evaluating thing env =
    match thing
    with Cons (func, args) ->
           (match (evaluating func env)
            with Closure(pars, body, bodyEnv) ->
                   apply pars args env body bodyEnv |
                 Primitive howTo ->
                   howTo args env |
                 _ ->
                   oops "Closure or Primitive expected") |
         Symbol name ->
           lookup env name |
         _ ->
           thing

(* APPLY. Apply a CLOSURE whose parameter list is PARS, whose argument list is
   ARGS, and whose body is BODY. Arguments in ARGS are evaluated in ARGS ENV,
   and BODY is evaluated in BODY ENV, after bindings for PARS are added to
   it. *)

  and apply pars args argsEnv body bodyEnv =
    let rec applying pars args bodyEnv =
      match (pars, args)
      with (Nil, Nil) ->
              evaluating body bodyEnv |
           (Nil, Cons (_, _)) ->
              oops "More arguments than parameters" |
           (Cons (_, _), Nil) ->
              oops "Fewer arguments than parameters" |
           (Cons (Symbol name, pars), Cons (arg, args)) ->
              applying pars args
                (envPut name (evaluating arg argsEnv) bodyEnv) |
           _ ->
              oops "Bad application"
    in applying pars args bodyEnv ;;

(* EVALUATE. Evaluate THING in the GLOBAL environment. *)

  let evaluate thing =
    evaluating thing [] ;;

(* IS MEMBER. Test if THING is a member of the Lisp list THINGS. *)

  let rec isMember thing things =
    match things
    with Cons (first, rest) ->
           thing = first || isMember thing rest |
         _ ->
           false ;;

(* ARE PARAMETERS. Test if THINGS is a Lisp list of SYMBOLs, in which no SYMBOL
   appears more than once. *)

  let rec areParameters things =
    match things
    with Nil ->
           true |
         Cons (first, rest) ->
           (match first
            with Symbol _ ->
                   not (isMember first rest) && areParameters rest |
                 _ ->
                   false) |
         _ -> false ;;

(* MAKE ARITHMETIC. Return a HOW TO function that takes two NUMBER arguments,
   evaluates them both, and computes a new NUMBER from them using the OCaml
   function OP. If that doesn't work then assert an error MESSAGE. *)

  let makeArithmetic op message =
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             (let left = evaluating left env
              in let right = evaluating right env
                 in match (left, right)
                    with (Number left, Number right) ->
                           Number (op left right) |
                         _ ->
                           oops message) |
           _ ->
             oops message) ;;

(* MAKE RELATION. Return a HOW TO function that takes two NUMBER arguments,
   evaluates them both, compares them using the OCaml function OP, and returns
   either NIL or T. If that doesn't work then assert an error MESSAGE. *)

  let makeRelation op message =
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             (let left = evaluating left env
              in let right = evaluating right env
                 in match (left, right)
                    with (Number left, Number right) ->
                           if op left right
                           then tee
                           else Nil |
                         _ ->
                           oops message) |
           _ ->
             oops message) ;;

(* PRIMITIVE. Bind a symbol with the string NAME to a PRIMITIVE that contains
   the OCaml function HOW TO. *)

  let primitive name howTo =
    global := envPut name (Primitive howTo) (! global) ;;

(* *. Multiply two NUMBER arguments and return a NUMBER. We must write the name
   of the OCaml multiplication function with extra blanks so it won't be read
   as a comment. *)

  primitive "*" (makeArithmetic ( * ) "* expected 2 NUMBERs") ;;

(* +. Add two NUMBER arguments and return a NUMBER. *)

  primitive "+" (makeArithmetic (+) "+ expected 2 NUMBERs") ;;

(* -. Negate a single NUMBER argument, or subtract two NUMBER arguments. Return
   a NUMBER. *)

  primitive "-"
    (fun args env ->
      match args
      with Cons (right, Nil) ->
             (match (evaluating right env)
              with Number right ->
                     Number (- right) |
                   _ ->
                     oops "- expected 1 or 2 NUMBERs") |
           Cons (left, Cons (right, Nil)) ->
             let left = evaluating left env
             in let right = evaluating right env
                in (match (left, right)
                    with (Number left, Number right) ->
                           Number (left - right) |
                         _ ->
                           oops "- expected 1 or 2 NUMBERs") |
           _ ->
             oops "- expected 1 or 2 NUMBERs") ;;

(* /. Divide two NUMBER arguments and return a NUMBER. If the second argument
   is 0 then assert an error instead. *)

  primitive "/"
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             let left = evaluating left env
             in let right = evaluating right env
                in (match (left, right)
                    with (Number 0, _) ->
                           oops "/ tried to divide by 0" |
                         (Number left, Number right) ->
                           Number (left / right) |
                         _ ->
                           oops "/ expected 2 NUMBERs") |
           _ ->
             oops "/ expected 2 NUMBERs") ;;

(* <, <=, <>, >, >=. Comparisons that take two NUMBERs and return T or NIL. *)

  primitive "<"  (makeRelation (<)   "< expected 2 NUMBERs") ;;
  primitive "<=" (makeRelation (<=) "<= expected 2 NUMBERs") ;;
  primitive "<>" (makeRelation (<>) "<> expected 2 NUMBERs") ;;
  primitive ">"  (makeRelation (>)   "> expected 2 NUMBERs") ;;
  primitive ">=" (makeRelation (>=) ">= expected 2 NUMBERs") ;;

(* =. Test if two ATOMs are equal and return T or NIL. *)

  primitive "="
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             let left = evaluating left env
             in let right = evaluating right env
                in (match (left, right)
                    with (Nil, Nil) ->
                           tee |
                         (Number _, Number _) | (Symbol _, Symbol _) ->
                           if left = right
                           then tee
                           else Nil |
                         (_, _) ->
                           oops "= expected 2 ATOMs") |
           _ ->
             oops "= expected 2 ATOMs") ;;

(* AND. If there are no arguments then return T. Otherwise evaluate arguments
   from left to right. If one returns NIL, then return NIL without evaluating
   the remaining arguments. Otherwise return the result of evaluating the last
   argument. *)

  primitive "and"
    (fun args env ->
      let rec anding args =
        match args
        with Nil ->
               tee |
             Cons (arg, Nil) ->
               evaluating arg env |
             Cons (arg, args) ->
               if (evaluating arg env) = Nil
               then Nil
               else anding args |
             _ ->
               oops "AND expected 0 or more THINGs"
      in anding args) ;;

(* ATOM. Test if the single argument is a NUMBER or a SYMBOL, returning either
   T or NIL. *)

  primitive "atom"
    (fun args env ->
      match args
      with Cons (arg, Nil) ->
             (match (evaluating arg env)
              with Number _ | Symbol _ ->
                     tee |
                   _ ->
                     Nil) |
           _ ->
             oops "ATOM expected 1 THING") ;;

(* CAR. Return the first element of a Lisp list. *)

  primitive "car"
    (fun args env ->
      match args
      with Cons (arg, Nil) ->
             (match (evaluating arg env)
              with Cons (first, _) ->
                     first |
                   _ ->
                     oops "CAR expected a CONS") |
           _ ->
             oops "CAR expected a CONS") ;;

(* CDR. Return a list without its first element. *)

  primitive "cdr"
    (fun args env ->
      match args
      with Cons (arg, Nil) ->
             (match (evaluating arg env)
              with Cons (_, rest) ->
                     rest |
                   _ ->
                     oops "CDR expected a CONS") |
           _ ->
             oops "CDR expected a CONS") ;;

(* CONS. Return a new list, whose first element is the first argument and whose
   remaining elements are in the second argument. *)

  primitive "cons"
    (fun args env ->
      match args
      with Cons (first, Cons (rest, Nil)) ->
             let first = evaluating first env
             in let rest = evaluating rest env
                in (match rest
                    with Cons (_, _) ->
                           Cons (first, rest) |
                         _ ->
                           oops "CONS expected a THING and a CONS") |
           _ ->
             oops "CONS expected a THING and a CONS") ;;

(* DEFINE. Take 2 arguments: a SYMBOL and a THING. Bind the unevaluated SYMBOL
   to the result of evaluating the THING, in the GLOBAL environment. Return the
   SYMBOL. *)

  primitive "define"
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             (match left
              with Symbol name ->
                     global := envPut name (evaluating right env) (! global) ;
                     left |
                   _ ->
                     oops "DEFINE expected a SYMBOL and a THING") |
           _ ->
             oops "DEFINE expected a SYMBOL and a THING") ;;

(* IF. Take 3 arguments: TEST, WHEN TRUE, and WHEN FALSE, all of which may be
   THINGs. First evaluate TEST. If it returns NIL then return the result of
   evaluating WHEN FALSE. If it returns any other THING then return the result
   of evaluating WHEN TRUE instead. *)

  primitive "if"
    (fun args env ->
      match args
      with Cons (test, Cons (whenTrue, Cons (whenFalse, Nil))) ->
             if (evaluating test env) = Nil
             then evaluating whenFalse env
             else evaluating whenTrue env |
           _ ->
             oops "IF expected 3 THINGs") ;;

(* OR. If there are no arguments then return NIL. Otherwise evaluate arguments
   from left to right. If one returns a THING that is not NIL, then return its
   result without evaluating the remaining arguments. Otherwise return the
   result of evaluating the last argument. *)

  primitive "or"
    (fun args env ->
      let rec oring args =
        match args
        with Nil ->
               Nil |
             Cons (arg, Nil) ->
               evaluating arg env |
             Cons (arg, args) ->
               let value = evaluating arg env
               in if value = Nil
                  then oring args
                  else value |
             _ ->
               oops "OR expected 0 or more THINGs"
      in oring args) ;;

(* LAMBDA, λ. Return a closure. Its parameter list is the unevaluated first
   argument, which must be a Lisp list of discrete SYMBOLs. Its body is the
   unevaluated second argument. If the current environment is GLOBAL, then the
   closure's environment is the empty environment. Otherwise the environment
   is the current one, in ENV. *)

  let howToLambda args env =
    match args
    with Cons (pars, Cons (body, Nil)) ->
           if areParameters pars
           then Closure (pars, body,
                  (if env == (! global)
                   then envMake ()
                   else env))
           else oops "LAMBDA or λ expected parameters and a body" |
         _ ->
           oops "LAMBDA or λ expected parameters and a body" ;;

(* LAMBDA and λ are synonyms, bound to the same function HOW TO LAMBDA. *)

  primitive "lambda" howToLambda ;;
  primitive "λ"      howToLambda ;;

(* LIST. Return a Lisp list whose elements are the result of evaluating all the
   arguments. If there are no arguments then return NIL. *)

  primitive "list"
    (fun args env ->
      let rec listing args =
        match args
        with Nil ->
               Nil |
             Cons (arg, args) ->
               Cons (evaluating arg env, listing args) |
             _ ->
               oops "LIST expected 0 or more THINGs"
      in listing args) ;;


  
 (*Number. The function number takes one argument, evaluates it, and tests if its result is a number (an integer).*)
  primitive "number"
  (fun args env ->
    let rec numbering args =
      match args 
      with 
        Nil -> Nil |
        Cons (arg, Nil) -> 
        (match (evaluating arg env)
          with
          Number x -> tee |
          _ -> Nil)  |       
        _ -> oops "NUMBER expected arguments in between 0 and 2"

    in numbering args) ;;

  

  (*Imply. In propositional logic, α → β is called an implication, and is read as ‘‘alpha implies beta.’’ *)
  primitive "imply"
    (fun args env ->
      let rec implying args =
        match args
        with Nil -> Nil |
            Cons (arg, Nil) -> evaluating arg env |
            Cons (arg, args) ->
              if (evaluating arg env) = Nil
              then tee
              else implying args |  
            _ -> oops "IMPLY expected more than 0 ARGUMENTS"
      in implying args) ;;


(*Let. The Lisp special form (let s e₁ e₂) acts like an OCaml let-expression.*)
  primitive "let"
    (fun args env ->
      let rec letting args =
        match args
        with 
        Cons(Symbol x,Cons(a,Cons(c,Nil))) -> evaluating c (envPut x (evaluating a env) env) |
        _ -> oops "Let expects a symbol"
      in letting args) ;; 
  


(* NOT. If the single argument is NIL, then return T, otherwise return NIL. *)
  primitive "not"
    (fun args env ->
      match args
      with Cons(arg, Nil) ->
             if (evaluating arg env) = Nil
             then tee
             else Nil |
           _ ->
             oops "NOT expected 1 THING") ;;

(* QUOTE. Return the single argument without evaluating it. *)

  primitive "quote"
    (fun args _ ->
      match args
      with Cons (thing, Nil) ->
             thing |
           _ ->
             oops "QUOTE expected 1 THING") ;;

end ;;

(* Tests for NUMBER. *)
(* ================= *)

(* (number nil) ⇒ Nil                                                 2 pts. *)

Evaluator.evaluate
  (Cons (Symbol "number", Cons (Nil, Nil))) ;;

(* (number (quote a)) ⇒ Nil                                           2 pts. *)

Evaluator.evaluate
  (Cons (Symbol "number",
     Cons (
       Cons (Symbol "quote", Cons(Symbol "a", Nil)),
       Nil))) ;;

(* (number (quote (a b c))) ⇒ Nil                                     2 pts. *)

Evaluator.evaluate
  (Cons (Symbol "number",
     Cons (
       Cons (Symbol "quote",
         Cons (
           Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil))),
           Nil)),
       Nil))) ;;

(* (number 7734) ⇒ Symbol "t"                                         2 pts. *)

Evaluator.evaluate
  (Cons (Symbol "number", Cons (Number 7734, Nil))) ;;

(* (number 0) ⇒ Symbol "t"                                            2 pts. *)

Evaluator.evaluate
  (Cons (Symbol "number", Cons (Number 0, Nil))) ;;

(* (number (+ 2 2)) ⇒ Symbol "t"                                      2 pts. *)

Evaluator.evaluate
  (Cons (Symbol "number",
     Cons (
       Cons (Symbol "+", Cons (Number 2, Cons (Number 2, Nil))),
       Nil))) ;;

(* Tests for IMPLY. *)
(* ================ *)

(* (imply t) ⇒ Symbol "t"                                             3 pts. *)

Evaluator.evaluate
  (Cons (Symbol "imply", Cons (Symbol "t", Nil))) ;;

(* (imply nil) ⇒ Nil                                                  3 pts. *)

Evaluator.evaluate
  (Cons (Symbol "imply", Cons (Nil, Nil))) ;;

(* (imply nil (/ 0 0)) ⇒ Symbol "t"                                   3 pts. *)

Evaluator.evaluate
  (Cons (Symbol "imply",
     Cons (Nil,
       Cons (
         Cons (Symbol "/", Cons (Number 0, Cons (Number 0, Nil))),
         Nil)))) ;;

(* (imply t nil (/ 0 0)) ⇒ Symbol "t"                                 3 pts. *)

Evaluator.evaluate
  (Cons (Symbol "imply",
    Cons (Symbol "t",
      Cons (Nil,
        Cons (
          Cons (Symbol "/", Cons (Number 0, Cons (Number 0, Nil))),
          Nil))))) ;;

(* (imply t t t 100) ⇒ Number 100                                     3 pts. *)

Evaluator.evaluate
  (Cons (Symbol "imply",
    Cons (Symbol "t",
      Cons (Symbol "t",
        Cons (Symbol "t",
          Cons (Number 100, Nil)))))) ;;

(* (imply (quote a) (quote b) (quote c) (quote z)) ⇒ Symbol "z"       3 pts. *)

Evaluator.evaluate
  (Cons (Symbol "imply",
    Cons (
      Cons (Symbol "quote", Cons (Symbol "a", Nil)),
      Cons (
        Cons (Symbol "quote", Cons (Symbol "b", Nil)),
        Cons (
          Cons (Symbol "quote", Cons (Symbol "c", Nil)),
          Cons (
            Cons (Symbol "quote", Cons (Symbol "z", Nil)),
            Nil)))))) ;;

(* Tests for LET. *)
(* ============== *)

(* (let n 1 n) ⇒ Number 1                                             2 pts. *)

Evaluator.evaluate
  (Cons (Symbol "let",
     Cons (Symbol "n",
       Cons (Number 1,
         Cons (Symbol "n", Nil))))) ;;

(* (let n 2 n) ⇒ Number 2                                             2 pts. *)

Evaluator.evaluate
  (Cons (Symbol "let",
    Cons (Symbol "n",
      Cons(Number 2,
        Cons (Symbol "n"
      , Nil))))) ;;

(* (let two 2 (+ two two)) ⇒ Number 4                                 2 pts. *)

Evaluator.evaluate
  (Cons (Symbol "let",
    Cons (Symbol "two",
      Cons (Number 2,
        Cons (
          Cons (Symbol "+", Cons (Symbol "two", Cons (Symbol "two", Nil))),
          Nil))))) ;;

(* (let two 2 (number 2)) ⇒ Symbol "t"                                2 pts. *)

Evaluator.evaluate
  (Cons (Symbol "let",
    Cons (Symbol "two",
      Cons (Number 2,
        Cons (
          Cons (Symbol "number", Cons (Symbol "two", Nil)),
          Nil))))) ;;

(* (let a 1 (let b 2 (+ a b))) ⇒ Number 3                             2 pts. *)

Evaluator.evaluate
  (Cons (Symbol "let",
    Cons (Symbol "a",
      Cons (Number 1,
        Cons (
          Cons (Symbol "let",
            Cons (Symbol "b",
              Cons (Number 2,
                Cons (
                  Cons (Symbol "+", Cons (Symbol "a", Cons (Symbol "b", Nil))),
                  Nil)))),
          Nil))))) ;;


(* TEST RESULTS


# # use "tests11.ml";;                       
type thing =
    Closure of thing * thing * environment
  | Cons of thing * thing
  | Nil
  | Number of int
  | Primitive of (thing -> environment -> thing)
  | Symbol of string
and environment = (string * thing) list
module type Evaluators = sig val evaluate : thing -> thing end
module Evaluator : Evaluators
- : thing = Nil
- : thing = Nil
- : thing = Nil
- : thing = Symbol "t"
- : thing = Symbol "t"
- : thing = Symbol "t"
- : thing = Symbol "t"
- : thing = Nil
- : thing = Symbol "t"
- : thing = Symbol "t"
- : thing = Number 100
- : thing = Symbol "z"
- : thing = Number 1
- : thing = Number 2
- : thing = Number 4
- : thing = Symbol "t"
- : thing = Number 3
# 
*)