// namespace CoreScript.Interpreter.RuntimeModules

// open CoreScript.Interpreter
// open CoreScript.Interpreter.Environment

// module Maybe =

//     let checkArgLength minLen (values: Environment.Value list) =
//         if minLen > values.Length then
//             failwith (sprintf "Insufficient arguments (expected: %i, given: %i)" minLen values.Length)

//     let just (self : Value) (call : NativeFunction<Value>) = 
//         match self with
//         | NoneVal -> 
//             checkArgLength 1 call.arguments
//             call.arguments |> List.head
//         | _ -> self

//     let isSome (self : Value) (_ : NativeFunction<Value>) = 
//         match self with
//         | NoneVal -> BoolVal false
//         | _ -> BoolVal true

//     let isNone (self : Value) (_ : NativeFunction<Value>) = 
//         match self with
//         | NoneVal -> BoolVal true
//         | _ -> BoolVal false

//     let createMaybe (call : NativeFunction<Value>) =
//         let value = 
//             match call.arguments |> List.tryItem 0 with
//             | Option.None
//             | Some( NoneVal ) ->
//                 NoneVal
//             | Some value -> value
//         let bind (call : NativeFunction<Value>) =


        

            
        