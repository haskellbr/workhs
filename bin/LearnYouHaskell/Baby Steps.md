Write a Haskell program that calculates the **sum** and **average** of its
arguments list.

This actually involves a lot of Haskell concepts, but since this is a workshop
based introduction, you'll be writting programs that interact with the
real-world from the start.

You program will be given a list of numbers and should output the **sum** and
**average** on separate lines:

    $ myprogram 1 2 3 4
    10
    2.5


## Tips
To read command-line arguments, you'll need the `getArgs` function, imported
from `System.Environment`. You test it out as so:

```haskell
import System.Environment
main = do
    as <- getArgs
    print as
```
