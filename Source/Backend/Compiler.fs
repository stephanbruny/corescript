namespace CoreScript.Compiler

open CoreScript
open CoreScript.Parser
open CoreScript.Interpreter
open CoreScript.Interpreter.Environment
open CoreScript.Tokens

open System.Reflection
open System.Reflection.Emit
open System
open CoreScript
open System.Reflection.Metadata

module Compiler =

    type CoreBuilder = {
        assemblyName : AssemblyName;
        assemblyBuilder : AssemblyBuilder;
        moduleBuilder : ModuleBuilder;
        typeBuilder : TypeBuilder;
    }

    let createCoreBuilder name =
        let asmName = AssemblyName()
        asmName.Name <- (name + "Assembly")
        let asmBuilder = AssemblyBuilder.DefineDynamicAssembly(asmName, AssemblyBuilderAccess.Run)
        let modBuilder = asmBuilder.DefineDynamicModule(name + "Module")
        let typeBuilder = modBuilder.DefineType(name, TypeAttributes.Public)
        {
            assemblyName = asmName;
            assemblyBuilder = asmBuilder;
            moduleBuilder = modBuilder;
            typeBuilder = typeBuilder;
        }


    type LocalScope = {
        outer : LocalScope option;
        arguments : Map<string, int>;
        mutable bindings : Map<string, LocalBuilder>;
        mutable functions : Map<string, MethodInfo>;
        hasReturn : bool;
    }

    let subScope scope hasReturn (argNames : string list) =
        {
            outer = Some scope;
            arguments = argNames |> List.mapi (fun i v -> (v, i)) |> Map.ofList;
            bindings = Map.empty;
            functions = Map.empty;
            hasReturn = hasReturn;
        }

    let eval moduleName (tokens : Token list) =
        
        let evalBuilder = createCoreBuilder moduleName
        let mainBuilder = evalBuilder.typeBuilder.DefineMethod("main", MethodAttributes.Public ||| MethodAttributes.Static, typeof<Object>, [||])
        
        let rec generate (scope : LocalScope) (gen : ILGenerator) (token : Token) =

            let rec executeCompiler lastResult (tokenList : Token list) =
                match tokenList with
                | [] -> lastResult
                | [lastToken] ->
                    let final : Type = generate scope gen lastToken
                    if scope.hasReturn then gen.Emit(OpCodes.Ret)
                    final
                | _ -> 
                    let last = generate scope gen tokenList.Head
                    executeCompiler last tokenList.Tail

            let getLocal name = 
                match scope.bindings.TryFind name with
                | Some index -> index
                | _ -> failwith (sprintf "Undeclared %s" name)
            
            // gen.BeginScope()
            printfn "Compile: %A (%A)" token scope.hasReturn
            match token with
            | Atom (String str) ->
                gen.Emit(OpCodes.Ldstr, str)
                typeof<string>
            | Atom (Integer i) -> 
                gen.Emit(OpCodes.Ldc_I4, i)
                gen.Emit(OpCodes.Box, typeof<int>)
                typeof<int>
                // gen.Emit(OpCodes.Box, typeof<int>)
            | Atom (Double d) -> 
                gen.Emit(OpCodes.Ldc_R4, (float32 d))
                gen.Emit(OpCodes.Box, typeof<float32>)
                typeof<float32>
            | Atom (Bool b) -> 
                match b with | true -> gen.Emit(OpCodes.Ldc_I4_1) | false -> gen.Emit(OpCodes.Ldc_I4_0)
                gen.Emit(OpCodes.Box, typeof<bool>)
                typeof<bool>
            | Atom (None) -> 
                gen.Emit(OpCodes.Ldnull)
                typeof<Object>
            | Atom (Name(Simple n)) -> 
                let maybeLoc = scope.bindings.TryFind n
                match maybeLoc with
                | Some loc -> 
                    gen.Emit(OpCodes.Ldloc, loc)
                    loc.LocalType
                | _ -> 
                    match scope.arguments |> Map.tryFind n with
                    | Some idx -> 
                        match idx with
                        | 0 -> gen.Emit(OpCodes.Ldarg_0)
                        | 1 -> gen.Emit(OpCodes.Ldarg_1)
                        | 2 -> gen.Emit(OpCodes.Ldarg_2)
                        | 3 -> gen.Emit(OpCodes.Ldarg_3)
                        | _ -> gen.Emit(OpCodes.Ldarg_S, (byte idx))
                        typeof<obj>
                    | _ -> failwith (sprintf "Unknown name %s" n)

                // gen.Emit(OpCodes.Dup)
            | Let (Atom (Name name), right) ->
                let letName = name |> Environment.nameToString
                let tp = generate scope gen right
                let lc = gen.DeclareLocal (tp)
                gen.Emit(OpCodes.Stloc, lc)
                // gen.Emit(OpCodes.Ldnull)
                scope.bindings <- scope.bindings |> Map.add letName lc
                tp
            | Return right ->
                generate scope gen right
            | InfixOperator (op, left, right) ->
                let tpLeft = generate scope gen left
                gen.Emit(OpCodes.Unbox_Any, tpLeft)
                let tpRight = generate scope gen right
                gen.Emit(OpCodes.Unbox_Any, tpRight)
                match op with
                | "+" -> gen.Emit(OpCodes.Add)
                | "-" -> gen.Emit(OpCodes.Sub)
                | "/" -> gen.Emit(OpCodes.Div)
                | "*" -> gen.Emit(OpCodes.Mul)
                | "%" -> gen.Emit(OpCodes.Rem)
                | "|" -> gen.Emit(OpCodes.Or)
                | "&" -> gen.Emit(OpCodes.And)
                | "^" -> gen.Emit(OpCodes.Xor)
                | "<<" -> gen.Emit(OpCodes.Shl)
                | ">>" -> gen.Emit(OpCodes.Shr)
                | "==" -> gen.Emit(OpCodes.Ceq)
                | "!=" -> 
                    gen.Emit(OpCodes.Ceq)
                    gen.Emit(OpCodes.Neg)
                | ">" -> gen.Emit(OpCodes.Cgt)
                | "<" -> gen.Emit(OpCodes.Clt)
                | _ -> failwith "Unknown op"
                gen.Emit(OpCodes.Box, tpLeft)
                tpLeft
            | Function (name, parameters, body) ->
                let fnName = name |> Environment.nameToString
                let paramArray = parameters |> List.map(fun _ -> typeof<obj>) |> Array.ofList
                let fnBuilder = 
                    if paramArray.Length > 0 then
                        evalBuilder.typeBuilder.DefineMethod(fnName, MethodAttributes.Public ||| MethodAttributes.Static, typeof<obj>, paramArray)
                    else
                        evalBuilder.typeBuilder.DefineMethod(fnName, MethodAttributes.Public ||| MethodAttributes.Static, typeof<obj>, null)
                let fnGen = fnBuilder.GetILGenerator()
                let argNames = parameters |> List.map Environment.nameToString
                let fnScope : LocalScope = subScope scope false argNames
                fnGen.Emit(OpCodes.Nop)
                let resultType = generate fnScope fnGen body
                fnGen.EmitWriteLine (sprintf "%s ()" fnName) 
                fnGen.Emit(OpCodes.Ret)
                scope.functions <- scope.functions |> Map.add fnName (fnBuilder :> MethodInfo)
                resultType
            // | Lambda (parameters, body) ->
            //     let fnBuilder = evalBuilder.typeBuilder.DefineMethod("", MethodAttributes.Public ||| MethodAttributes.Static)
            //     let fnGen = fnBuilder.GetILGenerator()
            //     parameters |> List.iteri(fun i _ -> fnGen.Emit(OpCodes.Ldarg, i))
            //     generate fnGen body
            //     let del = fnBuilder.CreateDelegate(typeof<Object>)
            //     gen.Emit(OpCodes.Ldobj, del.Method)
            | Block list -> 
                // gen.BeginScope ()
                let result = executeCompiler null list
                // gen.EndScope ()
                result
            | Call (Atom (Name (Simple n)), parameters) ->
                let fn = scope.functions |> Map.tryFind n
                match fn with
                | Some method ->
                    let argScope = { arguments = Map.empty; outer = scope.outer; bindings = scope.bindings; functions = scope.functions; hasReturn = false }
                    match parameters with
                    | Group list ->
                        let paramArray =
                            list |> List.mapi (fun i v ->
                                generate scope gen v |> ignore
                                typeof<obj>
                            )
                            |> Array.ofList
                        let fnParams = if paramArray.Length > 0 then paramArray else null
                        gen.EmitCall(OpCodes.Call, method, fnParams)
                    | _ -> 
                        generate argScope gen parameters |> ignore
                    method.ReturnType
                | _ -> failwith "Unknown function"
            | Group list -> 
                printfn "GRUOUP: %A" list
                list |> List.iteri (fun i v ->
                    generate scope gen v |> ignore
                )
                typeof<obj>
            | _ -> 
                gen.Emit(OpCodes.Nop)
                typeof<obj>
            // gen.EndScope()
        let mainGen = mainBuilder.GetILGenerator()
        let programScope = 
            {
                outer = Option.None;
                bindings = Map.empty;
                functions = Map.empty;
                arguments = Map.empty;
                hasReturn = false;
            }
        let resultType = generate programScope mainGen (tokens |> Tokens.Block)
        printfn "Type: %A" resultType
        // if (resultType <> null && resultType <> typeof<obj>) then 
        //     mainGen.Emit(OpCodes.Ldfld, resultType)
        mainGen.Emit(OpCodes.Ret)
        // mainGen.Emit(OpCodes.Box, resultType)
        // printfn "ASM: %s" tp.Assembly.
        try
            let tp = evalBuilder.typeBuilder.CreateType()
            let ptInstance = Activator.CreateInstance(tp, [||]);
            let res = tp.InvokeMember("main", BindingFlags.InvokeMethod, System.Type.DefaultBinder, ptInstance, [||])
            printfn "Result: %A" res
            ()
        with
        | ex -> printfn "%A" ex.InnerException


