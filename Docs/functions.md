[<< Previous](./variables.md) | [Index](./index.md) | [Next >>](./operators.md)

# Functions

Functions are building blocks, mapping a set of arguments to a result value.  
Unlike JavaScript, in CoreScript functions **are not objects**.  

## Define a Function

```corescript
function add(x, y) x + y;

function mul(x, y) {
    x * y;
}

function div(x, y) {
    return x / y;
}
```

All these examples are valid CoreScript-syntax.  
A function consists of a *name*, a set of arguments (or no arguments - `()`), followed by an expression or block.  

### Blocks

They contain a set of statements and expressions forming a *computation*.  
The last evaluated expression of a block is used as return value.  

### Return

As you can see from the examples above, `return` isn't always needed to represent the result of a block.  
Instead, the keyword can be used to build *early-returns*:

```corescript
function myEarlyReturner(something) {
    if (something == none) return "invalid";
    "valid";
}
``` 

Whenever a block evaluates to a `return`, it is immediatly exited.  
One could say `return` means: *exit this block with the following value...*

## Lambdas

Instead of the function-statement, functions can be defined anonymously (without name) within expressions:

```corescript
let lambdaAdd = fun (x, y) x + y;
let lambdaMul = fun (x, y) { x * y; };
let lambdaDiv = fun (x, y) { return x / y; };
```

Albeit, skipping the *name*, the same rules apply as to functions.