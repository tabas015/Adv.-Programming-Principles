



exception EvaluatorError;;


primitive "number"
  (fun args _ ->
    let rec numbering args =
      match args 
      with 
        Nil -> Nil |
        Cons (arg, Nil) -> evaluating arg env |
        _ -> raise EvaluatorError 

        ;;

primitive "imply"
    (fun args env ->
      let rec implying args =
        match args
        with Nil -> Nil |
            Cons (arg, Nil) -> evaluating arg env |
            Cons (arg, args) ->
              if (evaluating arg env) = Nil
              then args
              else implying args |  
            _ -> raise EvaluatorError 
      in implying args) ;;


primitive "let"
    (fun args env ->
      let rec letting args =
        match args
        with 
        (*3 args only *))
        _ -> raise EvaluatorError
      in letting args) ;;