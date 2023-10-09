# Lice
Lice language interpreter


## Roadmap
    - Parser
      - Variable definitions (done)
      - Binary operators (done)
      - Blocks of code (done)
      - Recursive functions and returns
      - Pattern matching
      - OCaml-like lists

## Idea of what a program could looks like

```
open Stdio

function main() = {
    let n = 20;
    for ()
}

```

```
open Math

-- Syracuse recursive function
function syracuse (n) = {
    if (n == 4 || n == 3 || n == 1) then return; end
    else
        print(n);
        match (n % 2) with {
            | 0 -> return syracuse(3 * n + 1);
            | 1 -> return syracuse(2 * n);
        }
    end
}

function main() = {
    let n = Math.random(1, 400);
    syracuse(n);
    return 0;
}
```
