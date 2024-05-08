<p align="center"><img src="https://raw.githubusercontent.com/gabyfle/Lice/main/lice.svg" width="200px" style="margin: auto; border-radius: 200px;"></p>

<h2 align="center">The Lice Programming Language</h2>

<p align="center"><img src="https://github.com/gabyfle/Lice/actions/workflows/workflow.yml/badge.svg?branch=main"> <img src="https://img.shields.io/github/v/release/gabyfle/Lice"> <img src="https://badgen.net/github/stars/gabyfle/Lice"></p>



## What is Lice ?

**Note:** *Lice is, before everything, a room for experimentation around interpreted programming languages.*
Lice is a little general purpose interpreted language. It's meant to be easy-to-learn when you know OCaml, C-like languages, and to be as easy to use as Lua.

## State of the project

Lice just recently switched from a *tree-walk* interpreter to a bytecode interpreter with its custom virtual machine. Features that worked before the switch are not garanteed to work yet, as this project is still under development. You can find below a list of features of the language. Features denoted with a *(TBD)* means that they are not working yet, or that it's not working as expected for the moment. <small>*(TDB = To Be Done)*</small>

This switch allowed Lice to gain in performance for code evalutation. The performances gained are both in memory and in time.

## Features of the language
- Type annotations
- Recursive functions
- OCaml-like pattern matching
- Native lists *(TBD)*
- Deconstructing inside pattern matching *(TBD)*
- Embedabble withing any OCaml application *(TBD)*
- First-class values functions
- OCaml bindings to write your own applications *(TBD)*

## Documentation

### Installing the Lice language library

To install the Lice language library, you'll need to have the `opam` dependency manager installed in your local system.

The first step is to clone locally the repository. You can use this command line to clone throught `https`:

```bash
git clone https://github.com/gabyfle/Lice.git
```

Then, navigate to your freshly created `Lice` folder and run the `opam install` command at the root of the project:

```bash
cd Lice
opam install .
```

The Lice language library is now installed into your system.

### Learning the Lice language

To have more accurate examples, as well as a fully detailed grammar diagram, see <a href="https://lice.gabyfle.dev">online documentation</a>. You can still find below some basic examples of the syntax of the Lice programming language.

For the moment, Lice isn't shipped with any built-in library.

#### Examples

##### Calculating the Euler constant (or exp(0))

```javascript
function factorial(n) {
    if (n == 0) return 1;

    return n * factorial(n - 1);
}

function exp_aux(n, acc) {
    if (n == 0) return acc;

    return exp_aux(n - 1, acc + (1  / factorial(n)));
}

function exp(n) {
    return exp_aux(n, 1);
}

let number e = exp(2);
```

## Licence

The Lice library is distributed under the Apache 2.0 licence. See the "LICENCE" file in the main repository to learn more about what you can do/what you can't do with it.
