# Lice
Lice language interpreter

## What is Lice ?

Lice is a little general purpose interpreted language. It's meant to be easy-to-learn when you know OCaml, C-like languages, and to be as easy to use as Lua.

The Lice language doesn't aim to be super-fast neither to have super-extensible standard library. It's primary goal is to be easy to extend for OCaml developer: creating a library written in OCaml for Lice should not be a pain in the ass.

## State of the project

For the moment, this is the very beginning of the project, the parser should work but hasn't been well tested yet, and the surrounding elements of what makes an interpreter aren't yet implemented/do not work yet. The project is still in the "WIP" stage.

Of course the ultimate goal, and also the primary feature of the language will be it's interoperability with OCaml, as well as its module system, to make OCaml developers able to build libraries for Lice in OCaml, but since we do not have something working yet, this will be in the last stage of the development.

## Licence

The Lice interpreter is distributed under the Apache 2.0 licence. See the "LICENCE" file in the main repository to learn more about what you can do/what you can't do with it.
