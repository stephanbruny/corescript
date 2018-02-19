namespace CoreScriptExample

open CoreScript
open CoreScript.Interpreter
open CoreScript.Interpreter.Environment


module Example =
    let hello (call : NativeFunction<Value>) =
        StringVal "Hello, from Extension!"

    let coreScriptExport = 
        [
            (Key "hello", NativeFunVal hello);
            (Key "text", StringVal "Hello from example lib!")
        ] |> Map.ofList |> TableVal