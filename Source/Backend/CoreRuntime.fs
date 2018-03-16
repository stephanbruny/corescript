namespace CoreScript.Compiler

open CoreScript
open CoreScript.Parser
open CoreScript.Interpreter
open CoreScript.Interpreter.Environment
open CoreScript.Tokens

open System
open System.Collections.Generic
open System.Dynamic
open System.Reflection
open System.Reflection.Emit
open System.Linq.Expressions
open System.Reflection.Metadata
open System.Collections
open CoreScript
open CoreScript

type CoreScope () as this = 
    let mutable local = new ExpandoObject();
    let mutable outer = new ExpandoObject();

    do
        let l = local :> IDictionary<_, _>
        l.["environment"] <- ref this
    member this.Local 
        with get () = local
        and set (value) = local <- value

    member this.Outer 
        with get () = outer
        and set (value) = outer <- value

    member this.JoinScopes () = 
        let localScope = this.Local |> Seq.cast<KeyValuePair<string, obj>>
        let outerScope = this.Outer |> Seq.cast<KeyValuePair<string, obj>>
        let result = new ExpandoObject() :> IDictionary<_, _>
        outerScope |> Seq.iter(fun kv -> result.[kv.Key] <- kv.Value)
        localScope |> Seq.iter(fun kv -> result.[kv.Key] <- kv.Value)
        result :?> ExpandoObject

    static member SubScope (outerScope : CoreScope) =
        let outer = outerScope.JoinScopes()
        let result = new CoreScope()
        result.Outer <- outer
        result

    member this.Get name =
        printfn "Local: %A" local
        let l =  local :> IDictionary<_, _>
        if (l.ContainsKey name) then
            l.[name]
        else
            let o = outer :> IDictionary<_, _>
            if (o.ContainsKey name) then
                o.[name]
            else
                failwith (sprintf "Undefined name '%s'" name)

    member this.AssignLocal name value =
        let loc = local :> IDictionary<_, _>
        loc.[name] <- value



type CoreClosure (scope : CoreScope ref, body : Expression, parameters : string list) =
    member this.Body = body
    member this.Parameters = parameters

    member this.Scope = scope
    member this.Invoke (eval : obj * CoreScope ref -> obj) (args : obj []) = 
        printfn "Invoke Args: %A" args
        let local = this.Scope.Value.Local :> IDictionary<_, _>
        this.Parameters |> List.iteri(fun i name ->
            match args |> Array.tryItem i with
            | Some v -> local.[name] <- v;
            | _ -> local.[name] <- null;
        )
        printfn "INVOKE SCOPE: %A" this.Scope.Value.Local
        eval (this.Body, this.Scope)

type CoreFunction (fn : Delegate) = 
    member this.Implementation = fn;

    member this.Call (args : obj []) =
        printfn "Impl: %A" this.Implementation
        this.Implementation.DynamicInvoke(args)

type CoreRuntime (scope : CoreScope) = 

    static member GetValue name (scope : CoreScope ref) = scope.Value.Get name

    static member LinqCompile (expression : Expression) =
        // If necessary, convert the expression to an object type.
        let expr =
            if (expression.Type <> typeof<obj>) then 
                Expression.Convert(expression, typeof<obj>) :> Expression
            else 
                expression

        // Wrap the expression in a lambda and compile into executable code
        let scopeParam = Expression.Parameter(typeof<CoreScope ref>, "environment")
        let lambdaExpression = Expression.Lambda<Func<CoreScope ref, Object>>(expr, scopeParam);
        lambdaExpression.Compile()

    member this.Scope = scope;

    static member SetLocal name (value : obj) (scope : CoreScope ref) = scope.Value.AssignLocal name value

    static member GenerateFunction name body parameters (scope : CoreScope ref) =
        printfn "Generate Function: %s" name
        let closure = new CoreClosure(scope, body, parameters)
        let result = new CoreFunction(new Func<obj * CoreScope ref -> obj, obj[], obj>(closure.Invoke))
        result

    static member CallFunction (fn : obj) (args : obj []) (scope : CoreScope ref) =
        printfn "CallFunction %A (%A)" fn args
        match fn with
        | :? System.Linq.Expressions.Expression as fnExp ->
            let lFn = CoreRuntime.LinqCompile fnExp
            let result = lFn.Invoke(scope)
            printfn "ExprFn: %A" result
            CoreRuntime.CallFunction result args scope
        | :? Delegate as dg -> dg.DynamicInvoke args
        | :? CoreFunction as coreFn ->
            coreFn.Call args
        | _ -> failwith "this value is not a function and cannot be applied"

    static member ApplyArithmetic (opObj : obj) (left : obj) (right : obj) =
        let applyOpi (op : string) (a : int) (b : int) =
            match op with
            | "+" -> a + b
            | "-" -> a - b
            | "*" -> a * b
            | "/" -> a / b
            | ">>" -> a >>> b
            | "<<" -> a <<< b
            | "^" -> a ^^^ b
            | "%" -> a % b
            | "|" -> a ||| b
            | "&" -> a &&& b
            | _ -> failwith (sprintf "Unknown operator %s" op)
        let applyOpf (op : string) (a : double) (b : double) =
            match op with
            | "+" -> a + b
            | "-" -> a - b
            | "*" -> a * b
            | "/" -> a / b
            | "%" -> a % b
            | _ -> failwith (sprintf "Unknown operator %s" op)
        let numType =
            if (left.GetType() = typeof<int>) && (right.GetType() = typeof<int>) then
                typeof<int>
            else
                if (left.GetType() = typeof<double>) || (right.GetType() = typeof<double>) then
                    typeof<double>
                else
                    if (left.GetType() = typeof<string>) && (right.GetType() = typeof<string>) then
                        typeof<string>
                    else
                        failwith (sprintf "Cannot apply %s on types %s, %s" (opObj.ToString()) (left.GetType().ToString()) (right.GetType().ToString()))
        
        match numType with
        | int -> applyOpi (string opObj) (left :?> int) (right :?> int) :> obj
        | double -> applyOpf (string opObj) (left :?> double) (right :?> double) :> obj
        | string -> 
            if (opObj :?> string) = "+" then
                (left :?> string) + (right :?> string) :> obj
            else
                failwith (sprintf "Cannot apply %s to string" (opObj.ToString()))