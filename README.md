# Julia interpreter

This is a small interpreter for a part of the Julia language.
It supports integers and boolean values.

## Running the interpreter

Run a Julia file:

```bash
runhaskell julia_interpreter.hs test.jl
```

Start the REPL:

```bash
runhaskell julia_interpreter.hs
```

Type `quit` to exit the REPL.

## Supported language features

Variables can store integers or boolean values:

```julia
x = 10
ok = true
```

The interpreter supports these arithmetic operators:

```text
+  -  *  /  div  mod
```

Both `/` and `div` use integer division.

It also supports comparison operators:

```text
<  <=  >  >=  ==  !=
```

Boolean expressions can use:

```text
!  &&  ||
```

Parentheses can be used in expressions:

```julia
result = (2 + 3) * 4
```

Values can be printed with `println`:

```julia
println(result)
```

The interpreter supports `if` with an optional `else`:

```julia
if x > 5
    println(true)
else
    println(false)
end
```

It supports `while` loops:

```julia
while x < 15
    x = x + 1
end
```

It supports `for` loops with an integer range. Both range ends are included:

```julia
for i = 1:3
    println(i)
end
```

The `let` block creates local variables:

```julia
let a = 3
    b = 4
    println(a * b)
end
```

A `let` block can also return a value:

```julia
println(let a = 10
    b = 20
    a + b
end)
```

## Built-in functions

The interpreter has three built-in functions:

- `abs(x)` returns the absolute value of an integer
- `min(a, b)` returns the smaller integer
- `max(a, b)` returns the larger integer

Example:

```julia
println(abs(-8))
println(min(4, 9))
println(max(4, 9))
```

Using an unknown variable, wrong value type, division by zero, or wrong function arguments gives a runtime error.
