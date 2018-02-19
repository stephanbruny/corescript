namespace CoreScript

open Lexer
open Tokens

module Expressions =

    [<Literal>]
    let OperatorAssign = "<-"
    [<Literal>]
    let OperatorMatchCase = "->"

    let getPrecedence (opToken : Lexer.Token) = 
        match opToken with
        // | TOperator "==" -> 8
        | TOperator "<"
        | TOperator ">"
        | TOperator ">="
        | TOperator "<=" -> 6
        | TOperator "&&"
        | TOperator "||" -> 0
        | TOperator "%" -> 2
        | TOperator "/" -> 2
        | TOperator "*" -> 2
        | TOperator "-" -> 1
        | TOperator "+" -> 1
        | _ -> 0

    let getKeyValueValue token =
        match token with
        | Block content -> Table content
        | _ -> token
        // | Atom _ -> token
        // | Lambda _ -> token
        // | Table _ -> token
        // | Collection _ -> token
        // | Async (Lambda _) -> token
        // | _ -> failwith (sprintf "Expected table value, but got %A" token)

    let getKeyValueKey token =
        match token with
        | Atom (Integer i) -> NumericKey i
        | Atom (String s) -> StringKey s
        | Atom (Name (Name.Simple n) ) -> NameKey n
        | CustomOperator op -> OperatorKey op
        | _ -> failwith (sprintf "Invalid table key: %A" token)

    let rec decomposeName results (tokens : (Lexer.Token * int) list) =
        let getResult result tail = 
            match result with 
            | [name] -> (Atom (Atomic.Name (Simple name)), tail)
            | _::_ -> (Atom (Atomic.Name (Subname (result |> List.rev) )), tail)
            | [] -> failwith "Name expected"
        match tokens with
        | (head, line)::tail ->
            match head with
            | TName name -> 
                if tail.IsEmpty then
                    getResult (name::results) []
                else
                    let (peek, _) = tail.Head
                    match peek with
                    | TPunct "." -> decomposeName (name::results) tail.Tail                    
                    | _ -> getResult (name::results) tail
            | _ -> getResult results tokens
        | [] -> getResult results []

    let expressionError message line = 
        failwith (sprintf "Error in expression (line %i): %s" line message)

    let checkTable tokens line =
        match tokens with
        | [] -> tokens
        | _ ->
            tokens |> List.iter(fun token ->
                match token with
                | MutableProperty (KeyValue (OperatorKey k, v))
                | KeyValue (OperatorKey k, v) ->
                    match v with
                    | Lambda _ -> ()
                    | Async (Lambda _) -> ()
                    | _ -> expressionError (sprintf "Operator (%s) lambda expected, but got %A" k v) line 
                | MutableProperty (KeyValue _) -> ()
                | Receiver _ -> ()
                | KeyValue _ -> ()
                | ParserError err -> failwith err 
                | _ -> expressionError (sprintf "Invalid token in table %A" token) line
            )
            tokens

    let rec expression parse (current : Lexer.Token, line) (tokenList : (Lexer.Token * int) list) =
        let parseOne token = 
            let (result, _) = parse [token]
            result
        let rec parseGroup (results : Token list) (tokens : (Lexer.Token * int) list) =
            let getResult rest = (Group(results |> List.rev), rest)
            match tokens with
            | (head, line)::tail ->
                match head with
                | TCloseBrace -> getResult tail
                | TPunct "," -> 
                    parseGroup results tail
                | _ -> 
                    let (value, rest) = expression parse (head, line) tail
                    parseGroup (value::results) rest
            | [] -> getResult []
        let getExpressionValue currentToken (tail : (Lexer.Token * int) list) =
            match currentToken with
            | TOpenBrace -> 
                let (group, rest) = parseGroup [] tail
                match group with
                | Group [res] -> (res, rest)
                | _ -> expressionError "Invalid Expression" line
                // let exprTokens = tail |> List.takeWhile (fun (t, _) -> t <> TCloseBrace)
                // if exprTokens.IsEmpty then
                //     (Group [], tail.Tail)
                // else
                //     let (result, _) = expression parse exprTokens.Head exprTokens.Tail
                //     (result, tail |> List.skip (exprTokens.Length + 1))
            | TString str -> (Atom (Atomic.String str), tail)
            | TInteger i -> (Atom (Atomic.Integer i), tail)
            | TDouble d -> (Atom (Atomic.Double d), tail)
            | TName name ->
                match name with
                | "unit" -> (Atom (Atomic.Unit), tail)
                | "none" -> (Atom (Atomic.None), tail)
                | "true" -> (Atom (Atomic.Bool true), tail)
                | "false" -> (Atom (Atomic.Bool false), tail)
                | "operator" ->
                    let (opToken, l) = tail.Head
                    match opToken with
                    | TOperator op -> (CustomOperator op, tail.Tail)
                    | _ -> expressionError (sprintf "Invalid operator definition (%A)" current) l
                | "mutable" ->
                    let (value, rest) = parse tail
                    (MutableProperty value, rest)
                | "ref" ->
                    let (value, rest) = parse tail
                    (Reference value, rest)
                | "receive" ->
                    let (value, rest) = parse tail
                    (Receiver value, rest)
                | "extend" ->
                    let (parent, n) = parse tail
                    let (value, rest) = expression parse n.Head n.Tail
                    match value with
                    | (Agent(Table _))
                    | Table _ -> (Extend (parent, value), rest)
                    | _ -> expressionError (sprintf "Can only extend tables (given %A)" value) line
                | "async" ->
                    let (res, rest) = expression parse tail.Head tail.Tail
                    (Async res, rest)
                | "fun" -> 
                    let (lookAhead, l) = tail.Head
                    if (lookAhead <> TOpenBrace) then expressionError "Expected lambda parameterts" l
                    let (parameters, n) = parseGroup [] tail.Tail
                    let (body, rest) = parse n
                    let paramNames = parameters |> groupToNames
                    (Lambda (paramNames, body), rest)
                | "agent" ->
                    let (block, rest) = parse tail
                    match block with
                    | Block content-> 
                        let table = (Table (checkTable content line))
                        (Agent table, rest)
                    | _ -> expressionError (sprintf "Expected table, but got %A" block) line
                | "function" -> 
                    let (nameTok, _) = tail.Head
                    let fnName =
                        match nameTok with
                        | TName n -> n
                        | _ -> expressionError  "Missing function name. Use 'fun' for lambda expressions" line
                    match tail.Tail.Head with
                    | (TOpenBrace, _) ->
                        let (parameters, rest) = parseGroup [] tail.Tail.Tail
                        let paramNames = parameters |> groupToNames
                        let (body, fin) = parse rest
                        (Function(Simple fnName, paramNames, body), fin)
                    | _ -> expressionError "Expected function parameters" line
                | "default" ->
                    let (body, rest) = parse tail
                    (DefaultCase body, rest)
                | _ -> 
                    let (name, rest) = decomposeName [] ((currentToken, line)::tail)
                    if not rest.IsEmpty then
                        let (peek, _) = rest.Head
                        match peek with 
                        | TOpenBrace ->
                            let (group, fin) = parseGroup [] rest.Tail
                            (Call(name, group), fin)
                        | _ -> (name, rest)
                    else 
                        (name, rest)
            | TOpenCurly ->
                let (block, restTail) = parse ((current, line)::tail)
                match block with
                | Block content -> (Table ( checkTable content line), restTail)
                | _ -> expressionError (sprintf "Invalid token in expression (%A)" block) line
            | TOperator op -> 
                let (right, rest) = parse tail
                (Prefix (op, right), rest)
            | _ -> parse tail
        let rec parsePrecedence (left : Token) (toks : (Lexer.Token * int) list) (minPrec : int) =
            match toks with
            | head::tail ->
                let (current, l) = head
                let prec = getPrecedence current
                if prec < minPrec then
                    (left, toks)
                else
                    let nextPrec = if current = TOperator "=" then prec else prec + 1
                    match current with
                    | TCloseCurly
                    | TCloseBrace -> (left, toks)
                    | TPunct ","
                    | TCloseBracket
                    | TPunct ";" -> (left, tail)
                    | TOpenBrace -> 
                        let (sub, rest) = parseGroup [] tail
                        match sub with
                        | Group [res] -> parsePrecedence res rest minPrec
                        | _ -> expressionError "Invalid expression" line
                    | TOperator op ->
                        if op = "=" || op = "<-" then
                            let (right, rest) = parse tail
                            let value = InfixOperator(op, right, left)
                            (value, rest)
                        else
                            let (nextTok, _) = tail.Head
                            let (next, exprRest) = getExpressionValue nextTok tail.Tail
                            let (nextValue, rest) =
                                match next with
                                | Atom (Name _) when not (tail.IsEmpty || tail.Tail.IsEmpty) -> 
                                    let (th, _) = tail.Tail.Head
                                    if th = TOpenBrace then
                                        parse tail
                                    else
                                        (next, exprRest)
                                | _ -> 
                                    (next, exprRest)
                            let (right, fin) = parsePrecedence nextValue rest nextPrec
                            let value = InfixOperator(op, left, right)
                            parsePrecedence value fin 0
                    | _ -> expressionError (sprintf "Operator expected (%A)" current) l
            | [] -> (left, toks)

        let rec parseLookAhead value (rest : (Lexer.Token * int) list) =
            if (rest.IsEmpty) then
                (value, [])
            else
                let (peek, _) = rest.Head
                match peek with
                | TOpenBracket ->
                    let (right, fin) = expression parse rest.Tail.Head rest.Tail.Tail
                    parseLookAhead (KeyAccess(value, right)) fin
                    // | _ -> expressionError (sprintf "Invalid key (%A)" list) line
                | TCloseBracket -> (value, rest.Tail)
                | TOpenBrace ->
                    let (group, fin) = parseGroup [] rest.Tail
                    parseLookAhead (Call(value, group)) fin
                | TPunct "." ->
                    let (prop, fin) = expression parse rest.Tail.Head rest.Tail.Tail
                    (Access(value, prop), fin)
                | TPunct(":") ->
                    // table
                    try
                        let key = getKeyValueKey value
                        let (tableValue, fin) = parse rest.Tail
                        (KeyValue (key, getKeyValueValue tableValue), fin)
                    with ex -> ((parseError ex.Message line), rest)
                | TName "as" ->
                    let (asName, fin) = parse rest.Tail
                    (As(value, asName), fin)
                | TOperator OperatorAssign -> 
                    let (right, fin) = parse rest.Tail
                    (Mutate(value, right), fin)
                | TOperator _ -> parsePrecedence value rest 0
                | TPunct ";"
                | TPunct "," -> (value, rest.Tail)
                | _ -> (value, rest)
        let (value, rest) = getExpressionValue current tokenList
        parseLookAhead value rest