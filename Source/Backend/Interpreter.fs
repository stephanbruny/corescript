namespace CoreScript.Interpreter

open CoreScript.Interpreter.Environment
open CoreScript
open CoreScript.Parser
open System
open System.IO
open CoreScript.Tokens

module Interpreter = 

    let getGroupedNames (list : Token list) =
        list |> List.map(fun tok ->
            match tok with
            | Atom (Name n) -> n |> Environment.nameToString
            | _ -> failwith (sprintf "Invalid name %A" tok)
        )

    let isTruthy (v : Value) =
        match v with
        | NoneVal -> false
        | BoolVal b -> b
        | IntVal i -> i <> 0
        | DoubleVal d -> d <> 0.0
        | _ -> true

    let compareGrater a b =
        match (a, b) with
        | (StringVal x, StringVal y) -> x > y
        | (IntVal x, IntVal y) -> x > y
        | (DoubleVal x, DoubleVal y) -> x > y
        | (BoolVal x, BoolVal y) -> x > y
        | _ -> false

    let compareGraterOrEqual a b =
        match (a, b) with
        | (StringVal x, StringVal y) -> x >= y
        | (IntVal x, IntVal y) -> x >= y
        | (DoubleVal x, DoubleVal y) -> x >= y
        | (BoolVal x, BoolVal y) -> x >= y
        | _ -> false

    let compareLess a b =
        match (a, b) with
        | (StringVal x, StringVal y) -> x < y
        | (IntVal x, IntVal y) -> x < y
        | (DoubleVal x, DoubleVal y) -> x < y
        | (BoolVal x, BoolVal y) -> x < y
        | _ -> false

    let compareLessOrEqual a b =
        match (a, b) with
        | (StringVal x, StringVal y) -> x <= y
        | (IntVal x, IntVal y) -> x <= y
        | (DoubleVal x, DoubleVal y) -> x <= y
        | (BoolVal x, BoolVal y) -> x <= y
        | _ -> false

    let plus a b =
        match (a, b) with
        | (StringVal x, StringVal y) -> [x; y] |> String.concat "" |> StringVal
        | (IntVal x, IntVal y) -> x + y |> IntVal
        | (IntVal x, DoubleVal y) -> (double x) + y |> DoubleVal
        | (DoubleVal x, IntVal y) -> x + (double y) |> DoubleVal
        | (DoubleVal x, DoubleVal y) -> x + y |> DoubleVal
        | (UnitVal, x) -> x
        | (x, UnitVal) -> x
        | _ -> ErrorVal (sprintf "Cannot add %A to %A" a b)

    let minus a b =
        match (a, b) with
        | (IntVal x, IntVal y) -> x - y |> IntVal
        | (IntVal x, DoubleVal y) -> (double x) - y |> DoubleVal
        | (DoubleVal x, IntVal y) -> x - (double y) |> DoubleVal
        | (DoubleVal x, DoubleVal y) -> x - y |> DoubleVal
        | (UnitVal, x) -> x
        | (x, UnitVal) -> x
        | _ -> ErrorVal (sprintf "Cannot substract %A from %A" b a)

    let multiply a b =
        match (a, b) with
        | (IntVal x, IntVal y) -> x * y |> IntVal
        | (IntVal x, DoubleVal y) -> (double x) * y |> DoubleVal
        | (DoubleVal x, IntVal y) -> x * (double y) |> DoubleVal
        | (DoubleVal x, DoubleVal y) -> x * y |> DoubleVal
        | (UnitVal, x) -> x
        | (x, UnitVal) -> x
        | _ -> ErrorVal (sprintf "Cannot multiply %A with %A" a b)
 
    let divide a b =
        match (a, b) with
        | (IntVal x, IntVal y) -> (double x) / (double y) |> DoubleVal
        | (IntVal x, DoubleVal y) -> (double x) / y |> DoubleVal
        | (DoubleVal x, IntVal y) -> x / (double y) |> DoubleVal
        | (DoubleVal x, DoubleVal y) -> x / y |> DoubleVal
        | (UnitVal, x) -> x
        | (x, UnitVal) -> x
        | _ -> ErrorVal (sprintf "Cannot divide %A by %A" a b)

    let modulo a b =
        match (a, b) with
        | (IntVal x, IntVal y) -> IntVal (x % y)
        | (DoubleVal x, IntVal y) -> DoubleVal (x % (double y))
        | (IntVal x, DoubleVal y) -> DoubleVal ((double x) % y)
        | (DoubleVal x, DoubleVal y) -> DoubleVal (x % y)
        | _ -> ErrorVal (sprintf "Cannot divide %A by %A" a b)

    let getScopeValue (scope : Scope<Value>) name =
        match scope.local |> Map.tryFind name with
        | Some v -> v
        | Option.None  ->
            match scope.outer |> Map.tryFind name with
            | Some v -> v
            | _ -> ErrorVal (sprintf "Undefined entity %s" name)
    
    let getScopeValueVar (scope : Scope<Value>) name =
        match scope.local |> Map.tryFind name with
        | Some (Mutable v) -> v.Value
        | Some (AgentVal (a, _)) -> a
        | Some v -> v
        | Option.None ->
            match scope.outer |> Map.tryFind name with
            | Some (Mutable v) -> v.Value
            | Some (AgentVal (a, _)) -> a
            | Some v -> v
            | _ -> ErrorVal (sprintf "Undefined variable %s" name)

    let addLocal (scope : Scope<Value>) (name : string, value : Value) = 
        { outer = scope.outer; local = scope.local |> Map.add name value}

    let newScope (scope : Scope<Value>) =
        let outer = Map.fold (fun acc key value -> Map.add key value acc) scope.outer scope.local
        { outer = outer; local = Map.empty }

    let rec eval (scope : Scope<Value>) (token : Token) =
        
        let rec evalAsync (scope : Scope<Value>) (atoken : Token) =
            match atoken with
            | Await value -> 
                let asyncValue = async { 
                    let (_, result) = evalAsync scope value
                    match result with
                    | AsyncVal pending ->
                        return! pending
                    | _ -> return result
                }
                (scope, asyncValue |> Async.RunSynchronously)
            | Function (name, _, _) ->
                let (_, fn) = eval scope atoken
                let result = Task fn
                let resultName = name |> Environment.nameToString
                (addLocal scope (resultName, result), Task fn)
            | Lambda _ ->
                let (_, fn) = eval scope atoken
                (scope, Task fn)
            | _ -> eval scope atoken

        let getCallTarget (scope : Scope<Value>) (token : Token) =
            match token with
            | Atom (Name _) ->
                let (_, ref) = eval scope token
                match ref with
                | FunVal _ -> ref
                | NativeFunVal _ -> ref
                | Task _ -> ref
                | _ -> failwith (sprintf "%A is not a function and cannot be applied" token)
            | Lambda (parameterNames, body) ->
                let parameters = parameterNames |> List.map Environment.nameToString
                FunVal(Option.None, parameters, body, scope)
            | Function (name, parameterNames, body) ->
                let fnName = name |> Environment.nameToString
                let parameters = parameterNames |> List.map Environment.nameToString
                FunVal(fnName |> Some, parameters, body, scope)
            | _ -> failwith (sprintf "Invalid name %A" token)
        let rec evalBlock (blockScope : Scope<Value>) (tokens : Token list) (current : Value) = 
            match tokens with
            | [] -> (blockScope, current)
            | head::tail ->
                match head with
                | Block b -> evalBlock blockScope b current
                | _ ->
                    let (next, value) = eval blockScope head
                    match value with
                    | ReturnVal _ -> (next, value)
                    | _ -> evalBlock next tail value
        let rec getCallArguments (tok : Token) =
            match tok with
            | Atom _ -> [tok |> Environment.getValue scope]
            | Group list -> 
                list |> List.map (fun groupItem ->
                    let (_, value) = eval scope groupItem
                    value
                )
            | _ -> 
                let (_, value) = eval scope token
                [value]
        let rec findTableValue (last : Value) (nameList : string list) =
            match nameList with
            | [] -> 
                match last with
                | Mutable v -> v.Value
                | _ -> last
            | root::tail ->
                match last with
                | TableVal contents ->
                    let maybeContent = contents |> Map.tryFind (JTableKey.Key root)
                    if maybeContent.IsNone then
                        findTableValue (NoneVal) tail
                    else
                        findTableValue (maybeContent |> Option.get) tail
                | _ -> failwith (sprintf "%A is not a table" last)
        let findNameValue getScopeValFn (ref : Name) =
            match ref with
            | Simple _ -> (scope, getScopeValFn scope (ref |> Environment.nameToString))
            | Subname names ->
                match names with
                | root::tail ->
                    let table = 
                        let v = getScopeValFn scope root
                        match v with
                        | AgentVal (table, _) -> table
                        | _ -> v
                    match table with
                    | TableVal _ -> 
                        let resultValue = findTableValue table tail
                        match resultValue with
                        | FunVal (a, b, c, localScope) ->
                                let tableScope = localScope.local |> Map.add "this" table
                                (scope, FunVal(a, b, c, { outer = localScope.outer; local = tableScope }))
                        | _ -> (scope, resultValue)
                    | _ -> failwith (sprintf "%A is not a table (%s)" (table.GetType()) root)
                | [] -> failwith "Invalid name"
        let evalFunction fnRef fnArgValues (fnName, fnParams, fnBody, (localScope : Scope<Value>)) =
            let isAsync = match fnRef with | Task _ -> true | _ -> false
            let fnScopeRef =
                match fnName with
                | Some name -> Some (name, fnRef)
                | _ -> Option.None
            let fnArgs = 
                fnParams 
                |> List.mapi(fun i argName ->
                    match fnArgValues |> List.tryItem i with
                    | Some v -> (argName, v)
                    | _ -> (argName, NoneVal)
                    )
                |> Map.ofList
            let fnScope = 
                let callScope = newScope localScope
                let localFnScope =
                    let resultScope = Map.fold (fun acc key value -> Map.add key value acc) fnArgs callScope.local // fnArgs |> Environment.joinMap (Environment.joinMap localScope scope)
                    if fnScopeRef.IsSome then
                        let (fnRecName, fnRec) = fnScopeRef |> Option.get
                        resultScope |> Map.add fnRecName fnRec
                    else
                        resultScope
                { outer = callScope.outer; local = localFnScope }
            let (_, result) = 
                if isAsync then
                    let result = async { 
                        let (_, res) = evalAsync fnScope fnBody 
                        return res
                    }
                    result |> Async.Ignore |> Async.Start
                    (scope, AsyncVal result)
                else
                    eval fnScope fnBody
            match result with
            | ReturnVal v -> (scope, v)
            | _ -> (scope, result)
        match token with
        | Atom _ -> 
            let result = Environment.getValue scope token
            match result with
            | RefVal ref -> findNameValue getScopeValueVar ref
            | _ -> (scope, result)
        | Table tableContents ->
            let keyValsList = tableContents |> List.map(fun tok ->
                match tok with
                | Receiver (KeyValue(k, v))
                | MutableProperty (KeyValue(k, v))
                | KeyValue (k, v) ->
                    let tableKey =
                        match k with
                        | NumericKey i -> JTableKey.IntKey i
                        | NameKey str
                        | OperatorKey str
                        | StringKey str -> JTableKey.Key str
                    let (_, tableValue) = eval scope v
                    match tok with
                    | MutableProperty _ -> 
                        (tableKey, Mutable (ref<Value> tableValue) )
                    | Receiver _ ->
                        (tableKey, ReceiverVal tableValue )
                    | _ -> (tableKey, tableValue)
                | _ -> failwith (sprintf "Invalid Table property (%A)" tok)
            )
            (scope, TableVal (keyValsList |> Map.ofList))
        | Agent tok -> 
            let (_, value) = eval scope tok
            match value with
            | TableVal _ ->
                let result = Environment.createAgent (value) (fun body args ->
                    match body with
                    | ReceiverVal ( FunVal (_, parameters, bodyToken, fnScope) ) ->
                        let argMap = 
                            parameters |> List.mapi(fun i name ->
                                let maybeVal = args |> List.tryItem i
                                match maybeVal with
                                | Some v -> (name, v)
                                | _ -> (name, NoneVal)
                            )
                            |> List.append( [ ("this", value) ] )
                        let receiverScope = Environment.joinMap fnScope.local (argMap |> Map.ofList)
                        let localScope = 
                            let newLocalScope = newScope scope
                            { outer = newLocalScope.outer; local = receiverScope }
                        try
                            eval localScope bodyToken |> ignore
                        with
                        | ex -> printfn "Unhandled error in receiver: %A" ex
                    | _ -> ()
                )
                (scope, result)
            | _ -> failwith (sprintf "Agents must be tables (given: %A)" (value.GetType()))
        | Return tok -> 
            let (_, result) = eval scope tok
            (scope, ReturnVal result)
        | Function (name, parameters, body) ->
            let fnName = name |> Environment.nameToString
            let fnVal = FunVal (Some fnName, (parameters |> List.map Environment.nameToString), body, scope)
            (addLocal scope (fnName, fnVal), fnVal)
        | Lambda (parameters, body) ->
            let fnVal = FunVal (Option.None, (parameters |> List.map Environment.nameToString), body, scope)
            (scope, fnVal)
        | Var (name, value) 
        | Let (name, value) ->
            match name with
            | Atom (Name n) ->
                let letName = n |> Environment.nameToString
                let existing = scope.local |> Map.tryFind letName
                if (existing.IsSome) then failwith (sprintf "%s has already been declared" letName)
                let (_, letVal) = eval scope value
                let newValue =
                    match token with
                    | Var _ -> Mutable (ref<Value> letVal)
                    | _ -> letVal
                ((letName, newValue) |> addLocal scope, UnitVal)
            | Group grp ->
                let names = grp |> getGroupedNames
                let (_, letValues) = eval scope value
                match letValues with
                | CollectionVal valueList ->
                    let mutable nextScope = scope.local
                    names |> List.iteri(fun i n ->
                        let nName = (box n) :?> string
                        let nVal = 
                            try
                                valueList.[i]
                            with 
                            | _ -> NoneVal
                        nextScope <- nextScope |> Map.add nName nVal
                    )
                    ({ outer = scope.outer; local = nextScope }, UnitVal)
                | _ -> failwith "Could not bind group item"
            | _ -> (scope, ErrorVal (sprintf "Invalid name %A" name))
        | Reference (tok) ->
            let result = Environment.getValue scope tok
            match result with
            | RefVal ref -> findNameValue getScopeValue ref
            | _ -> (scope, result)
        | Mutate (name, value) ->
            match name with
            | Atom (Name refName) ->
                match refName with
                | Simple _ ->
                    let key = (refName |> Environment.nameToString)
                    let target = getScopeValue scope key
                    match target with
                    | Mutable valRef ->
                        let (_, newValue) = eval scope value
                        valRef := newValue
                        (scope, UnitVal)
                    | _ -> failwith (sprintf "Cannot mutate immutable %s" key)
                | Subname nameList ->
                    let tableName = nameList |> List.rev |> List.tail |> List.rev
                    let propName = nameList |> List.last
                    let (_, table) = eval scope (Atom (Name (Subname tableName)))
                    match table with
                    | TableVal contents ->
                        let prop = contents |> Map.tryFind (Key propName)
                        if prop.IsNone then 
                            failwith (sprintf "Cannot find property %s" propName)
                        else
                            let p = prop |> Option.get
                            match p with
                            | Mutable variable ->
                                let (_, newValue) = eval scope value
                                variable := newValue
                                (scope, UnitVal)
                            | _ -> failwith (sprintf "Cannot mutate immutable %s" propName)
                    | _ -> failwith (sprintf "%s is not a table" (tableName |> String.concat "."))                    
            | _ -> failwith "Invalid mutation"
        | Call (fnToken, argsToken) ->
            let fnRef = 
                match fnToken with
                | Call _ -> 
                    let (_, subCall) = eval scope fnToken
                    subCall
                | _ -> getCallTarget scope fnToken
            let fnArgValues = argsToken |> getCallArguments
            match fnRef with
            | Task (FunVal (fnName, fnParams, fnBody, localScope))
            | FunVal (fnName, fnParams, fnBody, localScope) ->
                let callScope = newScope scope
                let fnScope = newScope localScope
                evalFunction fnRef fnArgValues (fnName, fnParams, fnBody, { outer = callScope.outer; local = fnScope.outer }) 
            | NativeFunVal nativeFn -> 
                let nativeEnv : NativeFunction<Value> = { arguments = fnArgValues; scope = scope; eval = eval}
                (scope, nativeFn (nativeEnv))
            | Task _ -> failwith "Invalid task"
            | _ -> failwith (sprintf "%A is not a function and cannot be applied (%A)" (fnRef.GetType()) fnToken)
        | Group group ->
            match group with
            | [Operator _] -> eval scope group.Head
            | _ ->
                let groupVals = 
                    group 
                    |> List.map (fun item -> 
                        let (_, result) = eval scope item
                        result
                    ) |> CollectionVal
                (scope, groupVals)
        | Block (tokens) ->
            evalBlock scope tokens NoneVal
        | Extend (parent, child) ->
            let (_, parentTable) = eval scope parent
            match parentTable with
            | TableVal parentContents ->
                let (_, childTable) = eval scope child
                match childTable with
                | TableVal childContents ->
                    let childProps = Map.fold (fun acc key value -> Map.add key value acc) parentContents childContents
                    (scope, (TableVal childProps))
                | _ -> failwith (sprintf "Cannot extend %A" childTable)
            | _ -> failwith (sprintf "Cannot extend from %A" parentTable)
        | Prefix (op, left) ->
            match op with
            | "-" ->
                let (_, a) = eval scope left
                let result =
                    match a with
                    | IntVal i -> IntVal -i
                    | DoubleVal d -> DoubleVal -d
                    | _ -> failwith "numeric value expected"
                (scope, result)
            | "!" ->
                let (_, a) = eval scope left
                let result =
                    match a with
                    | BoolVal b -> BoolVal (not b)
                    | _ -> BoolVal (not (isTruthy a))
                (scope, result)
            | "~" ->
                let (_, a) = eval scope left
                let result =
                    match a with
                    | IntVal i -> IntVal (~~~i)
                    | _ -> failwith "integer expected"
                (scope, result)
            | _ -> (scope, UnitVal)
        | InfixOperator (op, left, right) ->
            match op with
            | "==" -> 
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                (scope, BoolVal (compareValues a b))
            | "&&" -> 
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let boolResult = (a |> isTruthy) && (b |> isTruthy)
                (scope, BoolVal boolResult)
            | "||" -> 
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let boolResult = (a |> isTruthy) || (b |> isTruthy)
                (scope, BoolVal boolResult)
            | ">" -> 
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                (scope, BoolVal (compareGrater a b))
            | ">=" -> 
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                (scope, BoolVal (compareGraterOrEqual a b))
            | "<" -> 
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                (scope, BoolVal (compareLess a b))
            | "<=" -> 
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                (scope, BoolVal (compareLessOrEqual a b))
            | "!=" ->
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                (scope, BoolVal (not(compareValues a b)))
            | "+" ->
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let result = plus a b
                (scope, result)
            | "-" ->
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let result = minus a b
                (scope, result)
            | "*" ->
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let result = multiply a b
                (scope, result)
            | "/" ->
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let result = divide a b
                (scope, result)
            | "%" ->
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let result = 
                    match (a, b) with
                    | (IntVal x, IntVal y) -> IntVal (x % y)
                    | (DoubleVal x, IntVal y) -> DoubleVal (x % (double y))
                    | (IntVal x, DoubleVal y) -> DoubleVal ((double x) % y)
                    | (DoubleVal x, DoubleVal y) -> DoubleVal (x % y)
                    | _ -> failwith (sprintf "Invalid type for modulo (%%), numeric expected. {%A ; %A}" left right)
                (scope, result)
            | "|>" ->
                let (_, b) = eval scope right
                match b with
                | FunVal (fnName, fnParams, fnBody, fnScope) ->
                    let (_, a) = eval scope left
                    evalFunction b [a] (fnName, fnParams, fnBody, fnScope)
                | NativeFunVal fn ->
                    let (_, a) = eval scope left
                    let nativeCall : NativeFunction<Value> = { arguments = [a]; scope = scope; eval = eval }
                    (scope, fn nativeCall) 
                | _ -> failwith "operator |> can only be applied to a function"
            | "&" ->
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let result = 
                    match (a, b) with
                    | (IntVal x, IntVal y) -> IntVal (x &&& y)
                    | _ -> failwith "integer expected"
                (scope, result)
            | "|" ->
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let result = 
                    match (a, b) with
                    | (IntVal x, IntVal y) -> IntVal (x ||| y)
                    | _ -> failwith "integer expected"
                (scope, result)
            | "<<" ->
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let result = 
                    match (a, b) with
                    | (IntVal x, IntVal y) -> IntVal (x <<< y)
                    | _ -> failwith "integer expected"
                (scope, result)
            | ">>" ->
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let result = 
                    match (a, b) with
                    | (IntVal x, IntVal y) -> IntVal (x >>> y)
                    | _ -> failwith "integer expected"
                (scope, result)
            | "^" ->
                let (_, a) = eval scope left
                let (_, b) = eval scope right
                let result = 
                    match (a, b) with
                    | (IntVal x, IntVal y) -> IntVal (x ^^^ y)
                    | _ -> failwith "integer expected"
                (scope, result)
            | _ -> (scope, UnitVal)
        | If (condition, ifCase, elseCase) ->
            let (_, cond) = eval scope condition
            let evalCase case =
                match case with
                | Block b -> evalBlock scope b NoneVal
                | _ -> eval scope case
            if isTruthy cond then
                evalCase ifCase
            else
                match elseCase with
                | Some e -> evalCase e
                | Option.None -> (scope, UnitVal)
        | While (condition, body) ->
            let rec loop () =
                let (_, whileCondition) = eval scope condition
                if isTruthy whileCondition then
                    let (_, result) = eval scope body
                    match result with
                    | ReturnVal v -> v
                    | _ -> loop ()
                else
                    UnitVal
            (scope, loop ())
        | TryCatch (subject, catch) ->
            try
                eval scope subject
            with
            | ex ->
                match catch with
                | Catch (Atom(Name(Simple name)), body) ->
                    let catchScope = (name, (ErrorVal ex.Message)) |> addLocal scope
                    eval catchScope body
                | _ -> failwith ex.Message
        | Throw subject ->
            let (_, value) = eval scope subject
            match value with
            | StringVal str -> failwith str
            | _ -> failwith (sprintf "%A" value)
        | ParserError e -> 
            eprintfn "%s" e
            (scope, ErrorVal e)
        | Send (receiver, subject) ->
            match receiver with
            | Atom (Name n) ->
                let (_, agent) = findNameValue getScopeValue n
                match agent with
                | AgentVal (TableVal table, mailbox) ->
                    match subject with
                    | Call (Atom(Name(Simple messageName)), argumentsToken) ->
                        let (_, arguments) = eval scope argumentsToken
                        let argList = 
                            match arguments with
                            | CollectionVal l -> l
                            | _ -> [arguments]
                        let msg : ReceiverMessage<Value> = { subject = messageName; arguments = argList }
                        do mailbox.Post msg
                        (scope, UnitVal)
                    | _ -> (scope, UnitVal)
                | _ -> (scope, UnitVal)
            | _ -> failwith (sprintf "Invalid receiver name: %A" receiver)
        | Async value ->
            let (scope, v) = evalAsync scope value
            (scope, v)
        | Await _ -> evalAsync scope token
        | Access (left, right) ->
            let (_, leftValue) = eval scope left
            match leftValue with
            | TableVal _ ->
                let find = findTableValue leftValue
                match right with
                | Atom (Name (Simple n)) -> (scope, find [n])
                | Atom (Name (Subname n)) -> (scope, find n)
                | Call (Atom(Name propName), argsToken) -> 
                    let tableProperty = 
                        match propName with
                        | Simple n -> find [n]
                        | Subname n -> find n
                    let fnArgs = argsToken |> getCallArguments
                    match tableProperty with
                    | FunVal(fnName, parameters, body, fnScope) ->
                        let fnArgs = argsToken |> getCallArguments
                        let currentScope = fnScope.local |> Map.remove "this" |> Map.add "this" leftValue
                        let outer = newScope scope
                        evalFunction tableProperty fnArgs (fnName, parameters, body, { local = currentScope; outer = outer.outer }) 
                    | NativeFunVal fn ->
                        let nativeCall : NativeFunction<Value> = { arguments = fnArgs; scope = scope; eval = eval }
                        (scope, fn nativeCall)
                    | _ -> failwith (sprintf "%A is not a function and cannot be applied" propName)
//                | Call (Atom(Name (Subname n)), _) -> find n |> eval scope
                | _ -> 
                    printfn "%A <==> %A (%A)" left right leftValue
                    failwith "Invalid access"
            | _ -> failwith "Cannot access a value that is not a table"
        | KeyAccess (left, right) ->
            let (_, leftValue) = eval scope left
            let (_, rightValue) = eval scope right
            match leftValue with
            | TableVal contents ->
                let find = findTableValue leftValue
                match rightValue with
                | StringVal str -> (scope, find [str])
                | IntVal i ->
                    let result = contents |> Map.tryFind (IntKey i)
                    match result with
                    | Some res -> (scope, res)
                    | _ -> (scope, NoneVal)
                | _ -> failwith "Invalid key"
            | _ -> failwith "Cannot access a value that is not a table"
        | Import right ->
            let (_, rightVal) = eval scope right
            match rightVal with
            | StringVal str ->
                let (_, result) = Runtime.importModule scope eval str 
                (scope, result)
            | _ -> failwith "Invalid path for import"
        | Export right ->
            eval scope right
        | Match (subject, cases) ->
            let (_, compareValue) = eval scope subject
            let defaultCase = 
                let maybe = cases |> List.tryFind(fun tok -> match tok with | DefaultCase _ -> true | _ -> false)
                match maybe with
                | Some (DefaultCase t) -> Some t
                | _ -> Option.None
            let rec findMatch (tokenList : Token list) =
                match tokenList with
                | [] ->
                    match defaultCase with
                    | Some t -> eval scope t
                    | Option.None -> failwith "No match found"
                | h::t ->
                    match h with
                    | MatchCase (left, right) ->
                        match left with
                        | As (asLeft, asRight) ->
                            let (_, asCase) = eval scope asLeft
                            match asCase with
                            | NoneVal -> findMatch t
                            | _ ->
                                match asRight with
                                | Atom(Name (Simple n)) ->
                                    let caseScope = addLocal scope (n, asCase)
                                    let (_, result) = eval caseScope right
                                    (scope, result)
                                | _ -> failwith "Name expected after 'as"
                        | _ ->
                            let (_, compare) = eval scope left
                            if (compareValues compareValue compare) || (compareTables compareValue compare) then
                                eval scope right
                            else
                                findMatch t
                    | DefaultCase _ -> findMatch t
                    | _ -> failwith "Invalid expression in match"
            findMatch cases            
        | _ -> (scope, UnitVal)