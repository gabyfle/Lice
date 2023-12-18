# Lice
Lice language interpreter

## What is Lice ?

Lice is a little general purpose interpreted language. It's meant to be easy-to-learn when you know OCaml, C-like languages, and to be as easy to use as Lua.

The Lice language doesn't aim to be super-fast neither to have super-extensible standard library. It's primary goal is to be easy to extend for OCaml developer: creating a library written in OCaml for Lice should not be a pain in the ass.

## Why Lice ?

The first goal of Lice is to have something that is embeddable inside any OCaml project and that have a well defined and robust OCaml API. The main inspiration for this is, of course, the Lua programming language and it's very clean and easy-to-use C-API that enables C programmers to ship Lua into their C program easily.

As a long-term Lua developer and a newbie OCaml developer, I wanted to give a shot to this project and having a Lua-like language built directly into the OCaml ecosystem.

This project is also a way to learn more things about the way programming languages are crafted and built, as well as an opportunity to make a language that I know perfectly without even having to learn it, as all of it's syntax is inspired by C, OCaml and Lua, and that I'm familiar with these three languages.

## State of the project

For the moment, this is the very beginning of the project, the parser should work but hasn't been well tested yet, and the surrounding elements of what makes an interpreter aren't yet implemented/do not work yet. The project is still in the "WIP" stage.

Of course the ultimate goal, and also the primary feature of the language will be it's interoperability with OCaml, as well as its module system, to make OCaml developers able to build libraries for Lice in OCaml, but since we do not have something working yet, this will be in the last stage of the development.

## Features of the language
    - Type annotations
    - Recursive functions
    - OCaml-like pattern matching
    - Native lists
    - Embedabble withing any OCaml application

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

function power(x, n): number { -- you're not forced to use type annotations, they are optionnal
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

main();
```

#### Reverse a list

As you can see in this example, lists constructions are heavily inspired from the OCaml way of building and deconstructing lists. Here is a little snippet to show how you can reverse a list inside the Lice language.

```lua
function reverse_aux(l: list, acc: list): list {
    match l with {
        | [] -> return acc;
        | h :: t -> return reverse_aux(t, h :: acc);
    }
}

function reverse(l: list): list {
    return reverse_aux(l, []);
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

function main(x) {
    if (x == 10) { return x; }
    main(x + 1);
}

main(0);

```

## Licence

The Lice interpreter is distributed under the Apache 2.0 licence. See the "LICENCE" file in the main repository to learn more about what you can do/what you can't do with it.
