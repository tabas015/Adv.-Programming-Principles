

module type Parsers=
	sig 
	exception Can'tParse of string 
	val makeParser: string -> unit -> thing
end ;;



module Parser : Parsers =
struct
	
	exception Can'tParse of string ;;
	
	let can'tParse message =
    	raise (Can'tParse message) ;;


	let makeParser path =
		let nextToken =  Scanner.makeScanner path
	    in let token =  ref EndToken
	    in

    
    let nextThing () = 
		match !token 
		with
		CloseParenToken -> can'tParse "Close Paren Token" |
		EndToken  -> can'tParse "End Token" |
		NumberToken n -> token := nextToken() ; Number n |
		
		OpenParenToken  -> token := nextToken() ; nextThings() |
		
		SymbolToken s -> token := nextToken() ;
		(match s 
		 with "nil" -> Nil |
			  _ -> Symbol s) ;;
   
    
    
    let nextThings () = 
		match !token
		with CloseParenToken -> token := nextToken() ; Nil |
			 EndToken -> can'tParse "End Token" |
			 _ -> Cons(nextThing(), nextThings())


end ;;