0.0.1-dev (2024-05-07)
-------------------

This is the first version with a working prototype of the Lice interpreter using bytecode and a virtual machine instead of a tree-walk algorithm. Improvements have been made on memory usage and execution times.

### Modified

- Switched the whole interpreter to a bytecode interpreter. This should
  improve performance and reduce memory usage.

- Changed the way the lice library and the interpreter is compiled, making Lice a true OCaml library.

- Reformated the way types works inside the language and made an overall code cleanup.

- Functions are first class citizens.

### Fixed

- Fixed the lexing of float numbers.

Note: This is a pre-release and it's not intended to be used yet.
