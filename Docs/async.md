[<< Previous](./control-flow.md) | [Index](./index.md) | [Next >>](./agents.md)

# Asynchronous Programming

CoreScript supports asynchronous constructs, to write non-blocking functions.  

## Defining Async Functions

```corescript
async function foo () {
    let result = await bar();
    return { bar: result };
}

foo(); // unit
```

This example creates an asynchronous function.  

They behave differently from usual functions:  
If called, an async-function will immediatly return `unit`, but the function will be executed as a task in background.  

Async-functions can make use of the `await`-keyword, to receive results from other asynchronous functions.  
In this case, the function will block, until the value is returned.  
Because of actually beeing a background-task, this blocking will not interfere with the main-thread of your program.  

The *async*-feature is based on [.NET - Asynchronous Programming](https://docs.microsoft.com/en-US/dotnet/csharp/programming-guide/concepts/async/)

