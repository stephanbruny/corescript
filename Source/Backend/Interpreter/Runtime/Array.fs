namespace CoreScript.Interpreter.RuntimeModules

open CoreScript.Interpreter
open CoreScript.Interpreter.Environment

module Array =

    let checkArgLength minLen (values: Environment.Value list) =
        if minLen > values.Length then
            failwith (sprintf "Insufficient arguments (expected: %i, given: %i)" minLen values.Length)

    let foreachArray iteri (values : Value list) (call : NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        match call.arguments.Head with
        | Task (FunVal (_, parameters, body, fnScope))
        | FunVal (_, parameters, body, fnScope) ->
            values 
            |> Array.ofList
            |> iteri(fun idx v ->
                let callArgs = 
                    parameters |> List.mapi(fun i p ->
                        match i with
                        | 0 -> (p, IntVal idx)
                        | 1 -> (p, v)
                        | _ -> (p, NoneVal)
                    )
                    |> Map.ofList
                let callScope = Environment.joinMap fnScope.local callArgs
                call.eval { outer = fnScope.outer; local = callScope } body |> ignore
            )
            UnitVal
        | _ -> failwith "[Array.forEach] function expected"

    let headArray (values : Value list) (_ : NativeFunction<Value>) =
        values.Head


    let rec createArray (call : NativeFunction<Value>) =
        let createSub list = 
            let env : NativeFunction<Value> = { arguments = list; scope = call.scope; eval = call.eval }
            createArray env
        
        let mapArray mapi (values : Value list) (call : NativeFunction<Value>) =
            checkArgLength 1 call.arguments
            match call.arguments.Head with
            | Task (FunVal (_, parameters, body, fnScope))
            | FunVal (_, parameters, body, fnScope) ->
                    let resultArray =
                        values 
                        |> Array.ofList
                        |> mapi(fun idx v ->
                            let callArgs = 
                                parameters |> List.mapi(fun i p ->
                                    match i with
                                    | 0 -> (p, IntVal idx)
                                    | 1 -> (p, v)
                                    | _ -> (p, NoneVal)
                                )
                                |> Map.ofList
                            let callScope = Environment.joinMap fnScope.local callArgs
                            let (_, resultItem) = call.eval { outer = fnScope.outer; local = callScope } body
                            match resultItem with
                            | ReturnVal ret -> ret
                            | _ -> resultItem
                        )
                        |> List.ofArray
                    createArray { arguments = resultArray; scope = call.scope; eval = call.eval  }
            | _ -> failwith "[Array.forEach] function expected"

        let findArray (values : Value list) (call : NativeFunction<Value>) =
            checkArgLength 1 call.arguments
            match call.arguments.Head with
            | FunVal (_, parameters, body, fnScope) ->
                let result =
                    values 
                    |> List.tryFind(fun v ->
                        let callArgs = 
                            parameters |> List.mapi(fun i p ->
                                match i with
                                | 0 -> (p, v)
                                | _ -> (p, NoneVal)
                            )
                            |> Map.ofList
                        let callScope = Environment.joinMap fnScope.local callArgs
                        let (_, callResult) = call.eval { outer = fnScope.outer; local = callScope } body
                        match callResult with
                        | ReturnVal (BoolVal b)
                        | BoolVal b -> b
                        | _ -> false
                    )
                match result with
                | Some v -> v
                | Option.None -> NoneVal
            | _ -> failwith "[Array.find] function expected"

        let filterArray (values : Value list) (call : NativeFunction<Value>) =
            checkArgLength 1 call.arguments
            match call.arguments.Head with
            | FunVal (_, parameters, body, fnScope) ->
                let resultArray =
                    values 
                    |> List.filter(fun v ->
                        let callArgs = 
                            parameters |> List.mapi(fun i p ->
                                match i with
                                | 0 -> (p, v)
                                | _ -> (p, NoneVal)
                            )
                            |> Map.ofList
                        let callScope = Environment.joinMap fnScope.local callArgs
                        let (_, callResult) = call.eval { outer = fnScope.outer; local = callScope } body
                        match callResult with
                        | ReturnVal (BoolVal b)
                        | BoolVal b -> b
                        | _ -> false
                    )
                createSub resultArray
            | _ -> failwith "[Array.find] function expected"

        let sortArray (values : Value list) (call : NativeFunction<Value>) =
            checkArgLength 1 call.arguments
            match call.arguments.Head with
            | FunVal (_, parameters, body, fnScope) ->
                let resultArray =
                    values 
                    |> List.sortWith(fun a b ->
                        let callArgs = 
                            parameters |> List.mapi(fun i p ->
                                match i with
                                | 0 -> (p, a)
                                | 1 -> (p, b)
                                | _ -> (p, NoneVal)
                            )
                            |> Map.ofList
                        let callScope = Environment.joinMap fnScope.local callArgs
                        let (_, sortResult) = call.eval { outer = fnScope.outer; local = callScope } body
                        match sortResult with
                        | ReturnVal (IntVal -1)
                        | IntVal -1 -> -1
                        | ReturnVal (IntVal 1)
                        | IntVal 1 -> 1
                        | _ -> 0
                    )
                createSub resultArray
            | _ -> failwith "[Array.sort] function expected"
        
        let divideArray (values : Value list) (call : NativeFunction<Value>) =
            checkArgLength 1 call.arguments
            match call.arguments.Head with
            | IntVal i ->
                let parts = values |> List.splitInto i
                let subArrays = parts |> List.map createSub
                createSub subArrays
            | _ -> failwith "[Array.divide] integer expected"

        let skipArray (values : Value list) (call : NativeFunction<Value>) =
            checkArgLength 1 call.arguments
            match call.arguments.Head with
            | IntVal i ->
                let skipped = values |> List.skip i
                createSub skipped
            | _ -> failwith "[Array.skip] integer expected"

        let reverseArray (values : Value list) (_ : NativeFunction<Value>) =
            let result = values |> List.rev
            createSub result

        let appendArray (values : Value list) (call : NativeFunction<Value>) =
            let result = call.arguments |> List.append values
            createSub result

        let prependArray (values : Value list) (call : NativeFunction<Value>) =
            let result = values |> List.append call.arguments
            createSub result

        let sliceArray (values : Value list) (call : NativeFunction<Value>) =
            checkArgLength 1 call.arguments
            match call.arguments.Head with
            | IntVal i ->
                let start = i
                let skipped = values |> List.skip start
                let result =
                    if (call.arguments.Length > 1) then
                        match call.arguments.Tail.Head with
                        | IntVal j -> skipped |> List.take j
                        | _ -> failwith "[Array.slice] invalid slice count - integer expected"
                    else
                        skipped
                createSub result
            | _ -> failwith "[Array.slice] integer expected"

        let rec concatArray (values : Value list) (call : NativeFunction<Value>) =
            checkArgLength 1 call.arguments
            match call.arguments.Head with
            | TableVal table ->
                let indices = table |> Map.filter (fun k _ -> match k with | IntKey _ -> true | _ -> false)
                let concatValues = indices |> Map.toList |> List.map (fun (_, v) -> v)
                createSub (concatValues |> List.append values)
            | _ -> failwith "[Array.concat] table expected"

        let tailArray (values : Value list) (_ : NativeFunction<Value>) = createSub values.Tail
        let arrayValues = call.arguments |> List.mapi(fun i v -> (IntKey i, v))

        let arrayParallel = 
            [
                (Key "forEach", NativeFunVal (foreachArray Array.Parallel.iteri call.arguments));
                (Key "map", NativeFunVal (mapArray Array.Parallel.mapi call.arguments));
            ] |> Map.ofList |> TableVal

        let arrayMethods = 
            [
                (Key "length", IntVal arrayValues.Length);
                (Key "forEach", NativeFunVal (foreachArray Array.iteri call.arguments) );
                (Key "head", NativeFunVal (headArray call.arguments));
                (Key "tail", NativeFunVal (tailArray call.arguments));
                (Key "map", NativeFunVal (mapArray Array.mapi call.arguments));
                (Key "divide", NativeFunVal (divideArray call.arguments));
                (Key "skip", NativeFunVal (skipArray call.arguments));
                (Key "sort", NativeFunVal (sortArray call.arguments));
                (Key "reverse", NativeFunVal (reverseArray call.arguments));
                (Key "find", NativeFunVal (findArray call.arguments));
                (Key "filter", NativeFunVal (filterArray call.arguments));
                (Key "append", NativeFunVal (appendArray call.arguments));
                (Key "prepend", NativeFunVal (prependArray call.arguments));
                (Key "slice", NativeFunVal (sliceArray call.arguments));
                (Key "concat", NativeFunVal (concatArray call.arguments));
                (Key "parallel", arrayParallel);
            ] 
            |> Map.ofList
        let resultTable = Environment.joinMap (arrayValues |> Map.ofList) arrayMethods
        TableVal resultTable