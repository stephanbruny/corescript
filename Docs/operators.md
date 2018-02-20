[<< Previous](./functions.md) | [Index](./index.md) | [Next >>](./control-flow.md)

# Operators

## Unary Operators

| Expression | Name                | Description |
| ---------- | ------------------- | ----------- |
| `!x`       | negation            | Changes `true` to `false` and vice-verse
| `-x`       | unary minus         | additive inversion (integers and doubles)

## Infix Operators

| Expression   | Name                | Description |
| ----------   | ------------------- | ----------- |
| `x + y`      | plus                | performs arithmetic addition
| `x - y`      | binary minus        | performs arithmetic substraction
| `x * y`      | multiply            | performs arithmetic multiplication
| `x / y`      | divide              | performs arithmetic divison (casts to double)
| `x % y`      | modulo              | performs [euclidean devision](https://en.wikipedia.org/wiki/Euclidean_division)
| `x \|> F(x)` | pipe                | Pipes a value into a function(e.g. `let x = 3 \|> fun(n) n * n; // 9`)
| `x && y`     | logical AND         | performs logical AND (casting to boolean)
| `x \|\| y`   | logical OR          | performs logical OR (casting to boolean)

### Comparison

| Expression | Name                  | Description |
| ---------- | --------------------- | ----------- |
| `x == y`   | equality              | checks equality of both sides (including integer / double casting)
| `x > y`    | greater than          | checks left side to be greater in value than right side (integers and doubles)
| `x < y`    | less than             | checks left side to be less in value than right side (integers and doubles)
| `x >= y`   | greater than or equal | checks left side to be greater or equal in value than right side (integers and doubles)
| `x <= y`   | less than or equal    | checks left side to be less or equal in value than right side (integers and doubles)