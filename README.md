# Lice
Lice language interpreter

## What is Lice ?

Lice is a little general purpose interpreted language. It's meant to be easy-to-learn when you know OCaml, C-like languages, and to be as easy to use as Lua.

The Lice language doesn't aim to be super-fast neither to have super-extensible standard library. It's primary goal is to be easy to extend for OCaml developer: creating a library written in OCaml for Lice should not be a pain in the ass.

## State of the project

For the moment, this is the very beginning of the project, the parser should work but hasn't been well tested yet, and the surrounding elements of what makes an interpreter aren't yet implemented/do not work yet. The project is still in the "WIP" stage.

Of course the ultimate goal, and also the primary feature of the language will be it's interoperability with OCaml, as well as its module system, to make OCaml developers able to build libraries for Lice in OCaml, but since we do not have something working yet, this will be in the last stage of the development.

## Example programs

Here are some example programs. Note that since the current stage of the project is still "WIP", this syntax may be subject to slight changes.

#### Recursive power function

This little snippet shows how you can compute x^n when n is an integer. It uses a recursive function as well as pattern matching in order to achieve the computation.

```lua
--[[---------------------------
--    Recursive power func   --
--                           --
--    (this is a comment)    --
-----------------------------]]

function power(x, n): number {
    if (n == 0) { return 1; }
    match (x % 2) with {
        | 0 -> return power(x * x, n / 2);
        | 1 -> return power(x, n - 1);
    }
}

function main(): void {
    let number x = 30;
    let number n = 5;

    let number p = power(x, n);
}
```

#### Count to ten

This program demonstrate how to count to ten using a recursive function. Note: if we're using recursive functions for the moment, it's because we don't have any loop system in the language.

```lua
--[[---------------------------
--    Recursive power func   --
--                           --
--    (this is a comment)    --
-----------------------------]]

function main(x: number): number {
    if (x == 10) { return x; }
    main(x + 1);
}

main(0);

```

## Licence

The Lice interpreter is distributed under the Apache 2.0 licence. See the "LICENCE" file in the main repository to learn more about what you can do/what you can't do with it.
