<p align="center"><img src="https://github.com/gabyfle/Lice/blob/main/lice_frame.png?raw=true" width="150px" style="margin: auto; border-radius: 200px;"></p>

# Lice
Lice language main repository. Contains all the library code of the Lice language and everything you need to add Lice to your OCaml application.

## What is Lice ?

Lice is a little general purpose interpreted language. It's meant to be easy-to-learn when you know OCaml, C-like languages, and to be as easy to use as Lua.

## Features of the language
- Type annotations
- Recursive functions
- OCaml-like pattern matching
- Native lists
- Deconstructing inside pattern matching
- Embedabble withing any OCaml application
- First-class values functions
- OCaml bindings to write your own applications

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

## Licence

The Lice interpreter is distributed under the Apache 2.0 licence. See the "LICENCE" file in the main repository to learn more about what you can do/what you can't do with it.
