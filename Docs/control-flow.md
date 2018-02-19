[<< Previous](./operators.md) | [Index](./index.md) | [Next >>](./async.md)

# Control Flow

All control-flow-statements can be used as expressions, too.

## If/Else-Statement

```corescript
let something = if (someCondition()) true; else false;
let foo =
    if (bar == "bar") {
        "foobar";
    } else if (bar == "foo") {
        "foofoo";
    } else {
        "neither";
    };
```

## Match-Statement

This statement is similar to *switch*-Statements in other languages, but it can do more.  
The `match`-syntax allowes to compare results of expressions.  

Simple Example:  

```corescript
match (someString) {
    case ("foo") "it's a foo";
    case ("bar") "it's a bar";
    default "it's something else";
}
```

The simple example resembles a *switch*-statement.  
But *match* isn't limited to simple values:

Table Example:
```corescript
function createFooTable () {
    return { foo: 'bar' }
}

match (createFooTable()) {
    case ({ foo: 'bar' }) "yeah, a foo-table";
    case ({ bar: 'bar' }) "this is a bar-table";
    default "this is something else";
}
```

All kinds of data-structures can be matched.

### case-as

Let's take a look at a more complex use-case for *match*:

```corescript
match (req.path) {
    case (http.route("/hello/:name", req.path)) as route {
        let result = "Hello, " + route.name;
        res.sendText(result);
    };
    case (http.route("/hello/:name/:what", req.path)) as route {
        let result = "hey there, " + route.name + "! Say: " + route.what;
        res.sendText(result);
    };
    default { res.sendText("Hello, World!"); };
}
```

The `as`-syntax changes the behavior of `match`.  
Instead of comparing the exact value of the *case*-expression, it now evaluates the case when it returns everything but *none*  
and binds the result to a name.  
This name can be used in the cases right-hand expression for further computation.

**NOTE** it's always the first case, satisfying the match, which is evaluated.

### Default

When no case matches, the default-expression is evaluated.  
A missing `default` within a match without any matched cases, will lead to a runtime error.

## While-Loop
 
 Repeats an expression or block as long as a condition is true.

 ```corescript
 while (checkSomething()) {
     doSomething();
     if (someOtherCondition()) {
         return unit;
     }
 }
 ```

 As seen in this example, CoreScript has no `break`-keyword.  
 Instead, you can use `return` from within a block, to exit a loop.