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

module CompilerClr =
    let environmentParameter = Expression.Parameter(typeof<CoreScope ref>, "environment")

    let rec toClrType (value : Value) =
        match value with
        | StringVal str -> (str :> obj, typeof<string>)
        | IntVal i -> (i :> obj, typeof<int>)
        | DoubleVal d -> (d :> obj, typeof<double>)
        | BoolVal b -> (b :> obj, typeof<bool>)
        | TableVal t ->
            let o = new ExpandoObject() :> IDictionary<_, _>
            t |> Map.iter(fun k v ->
                let clrVal = toClrType v
                let keyName =
                    match k with
                    | IntKey i -> i.ToString()
                    | Key s -> s
                o.[keyName] <- clrVal
            )
            (o :> obj, typeof<obj>)
        | _ -> (null, typeof<obj>)

    let toConstant (value : obj, t : Type) = Expression.Constant(value, t)

    let compileInfix op (left : Expression) (right : Expression) =
        let opExpr = 
            match op with
            | "+"
            | "-"
            | "*"
            | "/"
            | ">>"
            | "<<"
            | "^"
            | "%"
            | "|"
            | "&" -> 
                let methodInfo = typeof<CoreRuntime>.GetMethod("ApplyArithmetic")
                Expression.Call(null, methodInfo, Expression.Constant(op), Expression.Convert(left, typeof<obj>), Expression.Convert(right, typeof<obj>)) :> Expression
            | "&&" -> Expression.AndAlso (left, right) :> Expression 
            | "||" -> Expression.OrElse (left, right) :> Expression
            | "==" -> Expression.Equal (left, right) :> Expression
            | "!=" -> Expression.NotEqual (left, right) :> Expression
            | ">" -> Expression.GreaterThan (left, right) :> Expression
            | ">=" -> Expression.GreaterThanOrEqual (left, right) :> Expression
            | "<" -> Expression.LessThan (left, right) :> Expression
            | "<=" -> Expression.LessThanOrEqual (left, right) :> Expression
        opExpr

    let linqCompile (expression : Expression) =
        // If necessary, convert the expression to an object type.
        let expr =
            if (expression.Type <> typeof<obj>) then 
                Expression.Convert(expression, typeof<obj>) :> Expression
            else 
                expression

        // Wrap the expression in a lambda and compile into executable code
        let lambdaExpression = Expression.Lambda<Func<CoreScope ref, obj>>(expr, environmentParameter);
        lambdaExpression.Compile()

    let getFunction parameters body = Expression.Lambda<Func<obj [], obj>>(body, parameters) :> Expression

    let rec compile (scope : CoreScope ref) (token : Token) =
        let runtime = new CoreRuntime(scope.Value)
        let rec eval (o : obj, evalScope : CoreScope ref) =
            printfn "eval: %A (%A)" (o.GetType()) evalScope.Value.Local
            match o with
            | :? Func<CoreScope ref, obj> as fn ->
                fn.Invoke(evalScope)
            | :? Expression<Func<CoreScope ref, obj>> as lambdaFn ->
                lambdaFn.Compile().Invoke(evalScope)
            | :? Expression as expr ->
                let res = linqCompile expr
                res.Invoke(evalScope)
            | _ -> o

        match token with
        | Atom ( String s ) -> toConstant (s, typeof<string>) :> Expression
        | Atom ( Integer i ) -> toConstant (i, typeof<int>) :> Expression
        | Atom ( Double d ) -> toConstant (d, typeof<double>) :> Expression
        | Atom ( Bool b ) -> toConstant(b, typeof<bool>) :> Expression
        | Atom ( Name (Simple n) ) ->
            let methodInfo = runtime.GetType().GetMethod("GetValue")
            Expression.Call(null, methodInfo, Expression.Constant(n), Expression.Constant(scope)) :> Expression
        | Let ( Atom (Name (Simple name)), value ) ->
            let valueCompiled = compile scope value |> linqCompile
            let result = valueCompiled.Invoke(scope)
            scope.Value.AssignLocal name result
            Expression.Constant((), typeof<unit>) :> Expression
        | InfixOperator (op, left, right) ->
            let leftValue = compile scope left
            let rightValue = compile scope right
            compileInfix op leftValue rightValue
        | Block toks ->
            let expressions = toks |> List.map(compile scope) |> Array.ofList
            Expression.Block expressions :> Expression
        | Function (name, parameters, body) ->
            let fnName = name |> Environment.nameToString
            let fnScope = CoreScope.SubScope scope.Value
            let args = parameters |> List.map (Environment.nameToString)
            args |> List.iter(fun n -> fnScope.AssignLocal n null)
            let fnBody = compile (ref fnScope) body
            let fnBodyExpr = Expression.Lambda<Func<CoreScope ref, obj>>(fnBody, environmentParameter);
            let methodInfo = runtime.GetType().GetMethod("GenerateFunction")
            let result = Expression.Call(null, methodInfo, Expression.Constant(fnName), Expression.Constant(fnBodyExpr), Expression.Constant(args), Expression.Constant(ref fnScope)) :> Expression
            scope.Value.AssignLocal fnName result
            result
        | Group toks ->
            let expressions = toks |> List.map(compile scope) |> List.map(fun expr -> Expression.Convert(expr, typeof<obj>) :> Expression)
            Expression.NewArrayInit(typeof<obj>, expressions |> Array.ofList) :> Expression
        | Call ( Atom (Name (Simple name)), (Group parameters)) ->
            let callArgs = 
                let expressions = parameters |> List.map(compile scope) |> List.map(fun expr -> Expression.Convert(expr, typeof<obj>) :> Expression)
                Expression.NewArrayInit(typeof<obj>, (expressions |> Array.ofList)) :> Expression
            let argExpressions = 
                [| 
                    Expression.Constant(eval, typeof<obj * CoreScope ref -> obj>) :> Expression;
                    callArgs
                |]
            printfn "ARGS: %A" argExpressions
            let args = Expression.NewArrayInit(typeof<obj>, argExpressions)
            let fn = scope.Value.Get name
            let methodInfo = runtime.GetType().GetMethod("CallFunction")
            Expression.Call(null, methodInfo, Expression.Constant(fn), args, Expression.Constant(scope)) :> Expression
            
            


