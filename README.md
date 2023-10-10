# Lice
Lice language interpreter


## Roadmap
    - Parser
      - Variable definitions (done)
      - Binary operators (done)
      - Blocks of code (done)
      - Recursive functions and returns (done)
      - Strong types in the language (WIP)
      - Maps (wip)
      - OCaml-like lists
      - for-loops, while-loops (wip)
      - Pattern matching


## Idea of what a program could looks like

```
open Stdio;

function main() = {
    let n = 20;
    for i=1, n {
        Stdio.print(i);
    }
}

let players = {
    "gabyfle" = true
};

function isPlayer(name: string): bool {
    return players[name];
}

--[[
    these are multiple lines comments
]]--
function main(): void {
    let bool isPly = isPlayer("gabyfle");
    if (!isPly)
        Stdio.print("Noooo");
    else
        Stdio.print("Yessss!");
}

main();

```

```
open Math;

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

main();

```
