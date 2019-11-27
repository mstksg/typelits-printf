# symbols-printf

(Heavily inspired by *[Data.Symbol.Examples.Printf][symbols]*)

[symbols]: https://hackage.haskell.org/package/symbols-0.3.0.0/docs/Data-Symbol-Examples-Printf.html

An extensible and type-safe printf from parsing GHC TypeLits Symbol literals,
matching the semantics of *[Text.Printf][]* in *base*.  The difference is that
the variants here will always fail to compile if given arguments of the wrong
type (or too many or too little arguments). Most of the variants also provide
useful type feedback, telling you the type of arguments it expects and how many
when queried with `:t` or with typed holes.

[Text.Printf]: https://hackage.haskell.org/package/base/docs/Text-Printf.html

Comparing their usage/calling conventions:

```haskell
ghci> putStrLn $ pprintf @"You have %.2f dollars, %s" (PP 3.62) (PP "Luigi")
You have 3.62 dollars, Luigi

ghci> putStrLn $ rprintf @"You have %.2f dollars, %s" (3.62 :% "Luigi" :% RNil)
You have 3.62 dollars, Luigi

ghci> putStrLn $ printf @"You have %.2f dollars, %s" 3.62 "Luigi"
You have 3.62 dollars, Luigi

-- printf also supports directly printing, like in Text.Printf
ghci> printf @"You have %.2f dollars, %s" 3.62 "Luigi"
You have 3.62 dollars, Luigi
```

Now comparing their types:

```haskell
ghci> :t pprintf @"You have %.2f dollars, %s" 3.62 "Luigi"
PP "f" -> PP "s" -> String

ghci> :t rprintf @"You have %.2f dollars, %s" 3.62 "Luigi"
FormatArgs '["f", "s"] -> String

ghci> :t printf @"You have %.2f dollars, %s" 3.62 "Luigi"
FormatFun '[ .... ] fun => fun
```

*   For `pprintf`, it shows you need two arguments: A `PP "f"` (which is a
    value that supports being formatted by `f`) like `PP 3.62`, and a `PP "s"`,
    like `PP "Luigi"`.
*   `rprintf` tells you you need a two-item hlist (from "Data.Vinyl.Core"),
    where the first item implements `f` and the second item implements `s`:
    `3.62 :% "Luigi" :% RNil` will do.
*   The type of `printf` doesn't tell you immediately what you
    you need.  However, if you do try to use it, the type errors will guide you
    along the way, iteratively.

    ```haskell
    ghci> printf @"You have %.2f dollars, %s"
    -- ERROR: Call to printf missing argument fulfilling "%.2f"
    -- Either provide an argument or rewrite the format string to not expect
    -- one.

    ghci> printf @"You have %.2f dollars, %s" 3.62
    -- ERROR: Call to printf missing argument fulfilling "%s"
    -- Either provide an argument or rewrite the format string to not expect
    -- one.

    ghci> printf @"You have %.2f dollars, %s" 3.62 "Luigi"
    You have 3.62 dollars, Luigi

    ghci> printf @"You have %.2f dollars, %s" 3.62 "Luigi" 72
    -- ERROR: An extra argument of type Integer was given to a call to printf
    -- Either remove the argument, or rewrite the format string to include the
    -- appropriate hole.
    ```

The following table summarizes the features and drawbacks of each
method:

| Method    | True Polyarity   | Naked Arguments    | Type feedback        |
| --------- | ---------------- | ------------------ | -------------------- |
| `pprintf` | Yes              | No (requires `PP`) | Yes                  |
| `rprintf` | No (HList-based) | Yes                | Yes                  |
| `printf`  | Yes              | Yes                | Partial (via errors) |

*Ideally* we would have a solution that has all three.  However, as of now, we
have a "pick two" sort of situation.  Suggestions are definitely welcome,
however, if you find something that satisfies all three benefits while still
allowing for polymorphism!

You can extend functionality with formatting for your own types by providing
instances of `FormatType`.

## Comparisons

There are a few other options for type-safe printfs out on hackage, and they
all differ in a few key ways.  Some, like *[th-printf][]* and
*[safe-printf][]*, offer Template Haskell-based ways to generate your printf
functions.  This package is intended as a "template-haskell free" alternative.
However, it is notable that with a Template-Haskell based approach, we can
solve the "pick two" situation above: *[th-printf][]*'s printf fulfills all
three requirements in the table above, with the only potential drawback being
Template Haskell usage.

Some others, like *[safe-printf][]*, *[formatting][]*, *[printf-safe][]*,
*[xformat][]*, and *[category-printf][]*, require manually constructing your
fomatters, and so you always need to duplicate double-quotes for string
literals.  This detracts from one of the main convenience aspects of *printf*,
in my opinion.

```haskell
"You have " % f' 2 % " dollars, " % s
-- vs
"You have %.2f dollars, %s"
```

However, calling these libraries "safe printf libraries" does not do them
justice.  A library like *[formatting][]* is a feature-rich formatting library,
handling things like dates and other useful formatting features in a
first-class way that embraces Haskell idioms.  This library here is merely a
type-safe printf, emulating the features of *base*'s printf and C `printf(3)`.

[th-printf]: https://hackage.haskell.org/package/th-printf
[safe-printf]: https://hackage.haskell.org/package/safe-printf
[formatting]: https://hackage.haskell.org/package/formatting
[printf-safe]: https://hackage.haskell.org/package/printf-safe
[xformat]: https://hackage.haskell.org/package/xformat
[category-printf]: https://hackage.haskell.org/package/category-printf

## Todo

*   Tests
*   Single-item formatters using *OverloadedLabels*
*   Support for localization/dynamic strings.  Should be possible, but we'd
    have to re-implement a subset of singletons.
