[<< Previous](./getting-started.md) | [Index](./index.md) | [Next >>](./variables.md)
# Types

CoreScript includes a set of simple base or native-types:

| Type       | Description |
| ---------- | ----------- |
| integer    | a real number (mathematical integer) |
| double     | a floating point number - double precision |
| boolean    | a binary logic value containing either *true* or *false* |
| string     | a sequence of characters (currently limited to UTF-8) |
| table      | a collection of name-value-pairs of any other type |
| none       | representing the absence of any value
| unit       | an empty value mostly used as return value of I/O-functions (like *void*)

## The Unit-Value

When returning from a function which is supposed to **not** return anything (e.g. I/O operations), instead of using `none` it returns `unit`.  
Thus signaling that the operation(s) inside the function is executed successfully.
Unit is the default return type of all functions.  


## The None-Value

None is returned whenever an expected value is absent.  
For example when trying to access a table-property which wasn't set before:

```corescript
let table = { foo: 'xyz' };
let bar = table.bar; // bar contains <none>
```

#### When to return None-Value?

You should return `none` whenever a function is supposed to return a value, but was unable to do so.  
It'll make it easier to distinguish between IO-functions and functions used in calculations.

```corescript
// Get a number within a given range
function getRange(min, max, n) {
    if (n >= min && n <= max) return n;
    return none;
}
```
