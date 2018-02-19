namespace CoreScript.Interpreter.RuntimeModules

open CoreScript
open CoreScript.Interpreter
open CoreScript.Interpreter.Environment

open System
open System.IO
open CoreScript
open CoreScript

module Tables =

    let checkArgLength minLen (values: Environment.Value list) =
        if minLen > values.Length then
            failwith (sprintf "Insufficient arguments (expected: %i, given: %i)" minLen values.Length)

    let rec keysTables (call : NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        let table = call.arguments.Head
        match table with
        | TableVal contents ->
            let keys = contents |> Map.toList |> List.map(fun (k, v) -> 
                match k with
                | Key s -> StringVal s
                | IntKey i -> IntVal i
            )
            RuntimeModules.Array.createArray { scope = call.scope; eval = call.eval; arguments = keys }
        | _ -> failwith "[Tables.keys] expected first parameter to be table"

    let pickTables (call: NativeFunction<Value>) =
        checkArgLength 2 call.arguments
        let table = call.arguments.Head
        match table with
        | TableVal contents ->
            let pickKeys = call.arguments.Tail |> List.map(fun v ->
                match v with
                | StringVal strKey -> Key strKey
                | IntVal iKey -> IntKey iKey
                | _ -> failwith "[Tables.pick] invalid key. Must be integer or string"
            )
            let picked = pickKeys |> List.map(fun pick ->
                let maybe = contents |> Map.tryFind pick
                match maybe with
                | Some value -> (pick, value)
                | _ -> (pick, NoneVal)
            )
            TableVal (picked |> Map.ofList)
        | _ -> failwith "[Tables.pick] expected first parameter to be table"

    let omitTables (call: NativeFunction<Value>) =
        checkArgLength 2 call.arguments
        let table = call.arguments.Head
        match table with
        | TableVal contents ->
            let omitKeys = call.arguments.Tail |> List.map(fun v ->
                match v with
                | StringVal strKey -> Key strKey
                | IntVal iKey -> IntKey iKey
                | _ -> failwith "[Tables.omit] invalid key. Must be integer or string"
            )
            let omitted = contents |> Map.filter(fun k _ -> not (omitKeys |> List.contains k))
            TableVal (omitted)
        | _ -> failwith "[Tables.omit] expected first parameter to be table"

    let valuesTables (call: NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        let table = call.arguments.Head
        match table with
        | TableVal contents ->
            let result = contents |> Map.filter(fun k v -> 
                match v with
                | FunVal _ -> false
                | NativeFunVal _ -> false
                | _ -> true
            )
            TableVal (result)
        | _ -> failwith "[Tables.values] expected first parameter to be table"

    let methodsTables (call: NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        let table = call.arguments.Head
        match table with
        | TableVal contents ->
            let result = contents |> Map.filter(fun k v -> 
                match v with
                | FunVal _ -> true
                | NativeFunVal _ -> true
                | _ -> false
            )
            TableVal (result)
        | _ -> failwith "[Tables.methods] expected first parameter to be table"

    let foreachTables (call : NativeFunction<Value>) =
        checkArgLength 2 call.arguments
        match call.arguments.Head with
        | TableVal contents ->
            match call.arguments.Tail.Head with
            | FunVal (_, parameters, body, fnScope) ->
                contents
                |> Map.iter(fun key value ->
                    let keyVal = match key with | Key s -> StringVal s | IntKey i -> IntVal i
                    let callArgs = 
                        parameters |> List.mapi(fun i p ->
                            match i with
                            | 0 -> (p, keyVal)
                            | 1 -> (p, value)
                            | _ -> (p, NoneVal)
                        )
                        |> Map.ofList
                    let callScope = Environment.joinMap fnScope.local callArgs
                    call.eval { outer = fnScope.outer; local = callScope } body |> ignore
                )
                UnitVal
            | _ -> failwith "[Tables.forEach] expected parameter 2 to be function"
        | _ -> failwith "[Tables.forEach] expected parameter 1 to be table"

    let mapTables (call : NativeFunction<Value>) =
        checkArgLength 2 call.arguments
        match call.arguments.Head with
        | TableVal contents ->
            match call.arguments.Tail.Head with
            | FunVal (_, parameters, body, fnScope) ->
                contents
                |> Map.map(fun key value ->
                    let keyVal = match key with | Key s -> StringVal s | IntKey i -> IntVal i
                    let callArgs = 
                        parameters |> List.mapi(fun i p ->
                            match i with
                            | 0 -> (p, keyVal)
                            | 1 -> (p, value)
                            | _ -> (p, NoneVal)
                        )
                        |> Map.ofList
                    let callScope = Environment.joinMap fnScope.local callArgs
                    let (_, result) = call.eval { outer = fnScope.outer; local = callScope } body
                    result
                ) |> TableVal
            | _ -> failwith "[Tables.map] expected parameter 2 to be function"
        | _ -> failwith "[Tables.map] expected parameter 1 to be table"

    let sizeTables (call: NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        let table = call.arguments.Head
        match table with
        | TableVal contents ->
            IntVal contents.Count
        | _ -> failwith "[Tables.methods] expected first parameter to be table"

    let containsTables (call: NativeFunction<Value>) =
        checkArgLength 2 call.arguments
        let table = call.arguments.Head
        let compare = call.arguments.Tail.Head
        match table with
        | TableVal contents ->
            let has = 
                contents 
                |> Map.toList 
                |> List.map (fun (_, v) -> v)
                |> List.tryFind(fun v ->
                    Environment.compareValues v compare || Environment.compareTables v compare
                )
            match has with
            | Some _ -> BoolVal true
            | Option.None -> BoolVal false
        | _ -> failwith "[Tables.contains] expected first parameter to be table"

    let tablesMethods = 
        [
            (Key "keys", NativeFunVal keysTables);
            (Key "pick", NativeFunVal pickTables);
            (Key "omit", NativeFunVal omitTables);
            (Key "values", NativeFunVal valuesTables);
            (Key "methods", NativeFunVal methodsTables);
            (Key "forEach", NativeFunVal foreachTables);
            (Key "map", NativeFunVal mapTables);
            (Key "size", NativeFunVal sizeTables);
            (Key "contains", NativeFunVal containsTables);
        ] 
        |> Map.ofList