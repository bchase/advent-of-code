# Usage

Create an `src/Day01/A.elm`:

```elm
module Day01.A exposing (main, solve)

import Helpers exposing (wrap)

main = wrap solve



solve : String -> String
solve = identity
```

Run:

```
$ make build && ./run 01 A < some-input-file
```
