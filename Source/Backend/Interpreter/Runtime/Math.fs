namespace CoreScript.Interpreter.RuntimeModules

open CoreScript.Interpreter
open CoreScript.Interpreter.Environment

open System

module Math =

    let checkArgLength minLen (values: Environment.Value list) =
        if minLen > values.Length then
            failwith (sprintf "Insufficient arguments (expected: %i, given: %i)" minLen values.Length)

    let getNumber (v : Value) =
        match v with
        | IntVal i -> float i
        | DoubleVal d -> float d
        | _ -> failwith "number expected"

    let convertAB a b (result : double) =
        match (a, b) with
        | (IntVal _, IntVal _) -> result |> int32 |> IntVal
        | (DoubleVal _, IntVal _) -> result |> DoubleVal
        | (IntVal _, DoubleVal _) -> result |> DoubleVal
        | (DoubleVal _, DoubleVal _) -> result |> DoubleVal
        | _ -> failwith "unexpected type"

    let convertA a (result : double) =
        match a with
        | IntVal _ -> result |> int32 |> IntVal
        | DoubleVal _ -> result |> DoubleVal
        | _ -> failwith "unexpected type"

    let mathAB fn (call : NativeFunction<Value>) =
        checkArgLength 2 call.arguments
        let a = call.arguments.Head
        let b = call.arguments.Tail.Head
        let x = getNumber a
        let y = getNumber b
        fn(x, y) |> convertAB a b

    let mathA fn (call : NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        let a = getNumber call.arguments.Head
        fn(a) |> convertA call.arguments.Head

    let mathAf fn (call : NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        let a = getNumber call.arguments.Head
        fn(a) |> double |> DoubleVal

    let mathAi fn (call : NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        let a = getNumber call.arguments.Head
        fn(a) |> int32 |> IntVal

    let atan2 (call : NativeFunction<Value>) = mathAB Math.Atan2 call
    let atan (call : NativeFunction<Value>) = mathA Math.Atan call
    let pow (call : NativeFunction<Value>) = mathAB Math.Pow call
    let abs (call : NativeFunction<Value>) = mathA Math.Abs call
    let ceil (call : NativeFunction<Value>) = mathAi Math.Ceiling call
    let floor (call : NativeFunction<Value>) = mathAi Math.Floor call
    let sqrt (call : NativeFunction<Value>) = mathAf Math.Sqrt call
    let cos (call : NativeFunction<Value>) = mathAf Math.Cos call
    let sin (call : NativeFunction<Value>) = mathAf Math.Sin call
    let tan (call : NativeFunction<Value>) = mathAf Math.Tan call
    let log (call : NativeFunction<Value>) = mathAf Math.Log call
    let acos (call : NativeFunction<Value>) = mathAf Math.Acos call
    let asin (call : NativeFunction<Value>) = mathAf Math.Asin call
    let pi (_ : NativeFunction<Value>) = Math.PI |> double |> DoubleVal
    let exp (call : NativeFunction<Value>) = mathA Math.Exp call
    let round (call : NativeFunction<Value>) = mathAi Math.Round call

    let mathTable = 
        [
            (Key "atan2", NativeFunVal atan2);
            (Key "atan", NativeFunVal atan);
            (Key "pow", NativeFunVal pow);
            (Key "abs", NativeFunVal abs);
            (Key "ceil", NativeFunVal ceil);
            (Key "floor", NativeFunVal floor);
            (Key "sqrt", NativeFunVal sqrt);
            (Key "cos", NativeFunVal cos);
            (Key "sin", NativeFunVal sin);
            (Key "tan", NativeFunVal tan);
            (Key "log", NativeFunVal log);
            (Key "acos", NativeFunVal acos);
            (Key "asin", NativeFunVal asin);
            (Key "pi", NativeFunVal pi);
            (Key "exp", NativeFunVal exp);
            (Key "round", NativeFunVal round);
        ] |> Map.ofList |> TableVal