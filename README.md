# Lice
Lice language interpreter


```
open Math

-- Syracuse recursive function
func syracuse (n) = {
    if (n == 4 || n == 3 || n == 1) then return; end
    else
        print(n);
        match (n % 2) with {
            | 0 -> return syracuse(3 * n + 1);
            | 1 -> return syracuse(2 * n);
        }
    end
}

func main() = {
    let n = Math.random(1, 400);
    syracuse(n);
    return 0;
}
```
