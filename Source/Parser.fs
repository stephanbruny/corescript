namespace CoreScript

open Lexer
open Tokens
open Expressions

module Parser =

    let rec parse (tokens : (Lexer.Token * int) list) = 
        let expectNext sym (tail : (Lexer.Token * int) list) =
            let (head, line) = tail.Head
            if (sym = head) then
                tail.Tail
            else
                failwith ( parserErrorMessage (sprintf "Expected %A, but got %A" sym tail.Head) line)
        let parseOne  (token : Lexer.Token, line : int) =
            let (result, _) = parse [(token, line)]
            result
        let rec parseUntil result (stopTokens : Lexer.Token list) (tokenList : (Lexer.Token * int) list) =
            match tokenList with
            | [] -> 
                failwith (sprintf "Missing %A" stopTokens)
                // (result |> List.rev, tokenList)
            | (head, line)::tail ->
                // if head = TPunct(";") then
                //     parseUntil result stopTokens tail
                // else
                if stopTokens |> List.contains head then
                    (result |> List.rev, tail)
                else
                    let (current, rest) = parse tokenList
                    parseUntil (current::result) stopTokens rest

        let parserError message line = 
            failwith (sprintf "Parser Error in line %i: %s" line message)
        
        match tokens with
        | [] -> (Atom Unit, [])
        | (head, line)::tail ->
            match head with
            /// Match Keywords
            | TOpenCurly ->
                let rec parseBlock (results : Token list) (tokenList : (Lexer.Token * int) list) =
                    match tokenList with
                    | [] -> (Block (results |> List.rev), [])
                    | h::t ->
                        match h with
                        | (TCloseCurly, _) -> (Block (results |> List.rev), t)
                        | (TComment, _) 
                        | (TPunct ",", _)
                        | (TPunct ";", _) ->
                            parseBlock (results) t
                        | _ ->
                            let (value, fin) = parse tokenList
                            match value with
                            | Atom Unit -> 
                                let r = 
                                    if fin.IsEmpty then 
                                        []
                                    else if fin.Tail.IsEmpty then
                                        []
                                    else fin.Tail
                                parseBlock(results) r
                            | _ -> parseBlock (value::results) fin
                parseBlock [] tail
            | TOpenBrace -> expression parse (head, line) tail
            | TName name ->
                match name with
                | "var" 
                | "let" ->
                    let Def =
                        if name = "var" then Var else Let
                    let (contents, rest) = parse tail
                    match contents with
                    | InfixOperator("=", value, def) -> 
                        match value with
                        | Block block -> 
                            let letValue = Table (checkTable block line)
                            ( Def(def, letValue), rest )
                        | _ -> ( Def(def, value), rest )
                    | _ -> parserError (sprintf "Expected let-binding but got %A" contents) line
                | "return" ->
                    let (result, rest) = expression parse tail.Head tail.Tail
                    (Return result, rest)
                | "default" ->
                    let (result, rest) = parse tail
                    (DefaultCase result, rest)
                | "import" ->
                    let (result, rest) = parse tail
                    (Import result, rest)
                | "export" ->
                    let (result, rest) = expression parse tail.Head tail.Tail
                    (Export result, rest)
                | "if" ->
                    let (case, rest) = parse tail
                    let (ifBody, next) = parse rest
                    let (elseBody, fin) =
                        let defaultResult = (Option.None, next)
                        if (next.IsEmpty) then
                            defaultResult
                        else
                            let (peek, n) = parse next
                            match peek with
                            | Else body -> (Some body, n)
                            | _ -> defaultResult
                    (If(case, ifBody, elseBody), fin)
                | "else" -> 
                    let (body, rest) = parse tail
                    (Else body, rest)
                | "break" -> (Break, tail)
                | "while" ->
                    let (case, rest) = parse tail
                    let (body, fin) = parse rest
                    (While(case, body), fin)
                | "foreach" ->
                    let (case, rest) = 
                        let (eachCase, next) = parse tail
                        match eachCase with
                        | Group [expr] -> (expr, next)
                        | _ -> parserError (sprintf "Expected foreach-case but got %A" eachCase) line
                    let (body, fin) = parse rest
                    (Foreach(case, body), fin)
                | "case" ->
                    let (left, next) = parse tail
                    let (right, rest) = parse next
                    (MatchCase(left, right), rest)
                | "match" ->
                    let (case, rest) = parse tail
                    let (body, fin) = parse rest
                    match body with
                    | Block contents ->
                        ( Match(case, contents), fin )
                    | _ -> parserError (sprintf "Expected match cases but got %A" body) line
                | "await" ->
                    let (result, rest) = parse tail
                    (Await result, rest)
                | "async" ->
                    let (result, rest) = parse tail
                    match result with
                    | Function _ 
                    | Block _
                    | Lambda _ -> (Async result, rest)
                    | _ -> parserError (sprintf "Expected function or lambda, but got %A" result) line
                | "throw" ->
                    let (result, rest) = parse tail
                    (Throw result, rest)
                | "try" ->
                    let (tryBody, n) = parse tail
                    let (catch, rest) = parse n
                    match catch with
                    | Catch _ -> (TryCatch (tryBody, catch), rest)
                    | _ -> parserError (sprintf "expected catch but found: %A" catch) line
                | "catch" ->
                    let (ex, n) = parse tail
                    match ex with
                    | Atom (Name (Simple _)) ->
                        let (body, rest) = parse n
                        (Catch(ex, body), rest)
                    | _ -> parserError (sprintf "expected delegate name, but found: %A" ex) line
                | "send" ->
                    let (receiver, n) = parse tail
                    let (message, rest) = parse n
                    (Send(receiver, message), rest)
                | _ -> 
                    let (result, rest) = expression parse (head, line) tail
                    (result, rest)
            // | TOperator _ -> expression parse (head, line) tail
            | TOperator _
            | TString _
            | TInteger _
            | TDouble _ -> expression parse (head, line) tail
            | _ -> parse tail

    let rec execute results scan =
        try
            match scan with
            | [] -> results |> List.rev
            | _ ->
                let (result, next) = parse scan
                execute (result::results) next
        with
        | ex -> 
            printfn "Exception: %A" ex
            []