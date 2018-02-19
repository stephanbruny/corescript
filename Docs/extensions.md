[<< Previous](./modules.md) | [Index](./index.md) | [Next >>](./embedding.md)

# Writing Extensions

CoreScript can import .NET-DLLs, when exposing a member `coreScriptExport` of Type `CoreScript.Interpreter.Environment.Value`.  

See the [ExampleLib](../Source/Extension) and also the [Runtime-Modules](../Source/Backend/Interpreter/Runtime).

The member `coreScriptExport` will be handled, like the result-value of a script-module.  
Most likely, you want to export a table or function. 