# symbols-printf

(Heavily inspired by *[Data.Symbol.Examples.Printf][symbols]*)

[symbols]: https://hackage.haskell.org/package/symbols-0.3.0.0/docs/Data-Symbol-Examples-Printf.html

```haskell
ghci> putStrLn $ printf @"You have %.2f dollars, %s" 3.62 "Luigi"
You have 3.62 dollars, Luigi
```

An extensible and type-safe printf from parsing GHC TypeLits Symbol literals,
matching the semantics of *[Text.Printf][]* in *base*.  The difference is that
your `printf`s will always fail to compile if given arguments of the wrong type
(or too many or too little arguments).  It also allows you to use types to help
your development, by telling you the type of arguments it expects and how many
when queried with `:t` or with typed holes.

[Text.Printf]: https://hackage.haskell.org/package/base/docs/Text-Printf.html

```haskell
ghci> putStrLn $ printf @"You have %.2f dollars, %s" 3.62 "Luigi"
You have 3.62 dollars, Luigi
```

Looking at its type:

```haskell
ghci> :t printf @"You have %.2f dollars, %s"
(FormatType "f" arg1, FormatType "s" arg2)
  => arg1 -> arg2 -> String
```

It tells you that the result is an `arg1 -> arg2 -> String`: take two
arguments, and return a `String`.  The first argument must be an instance of
`FormatType "f"` (things that can be formatted by `%f`) and the second argument
must be an instance of `FormatType "s"` (things that can be formatted by `%s`).

We can see this in action by progressively applying arguments:

```haskell
ghci> :t printf @"You have %.2f dollars, %s" 3.62
FormatType "s" arg1 => arg1 -> String

ghci> :t printf @"You have %.2f dollars, %s" 3.62 "Luigi"
String
```

The type errors for forgetting to apply an argument (or applying too many
arguments) are pretty clear:

```haskell
ghci> putStrLn $ printf @"You have %.2f dollars, %s"
-- ERROR: Call to printf missing argument fulfilling "%.2f"
-- Either provide an argument or rewrite the format string to not expect
-- one.

ghci> putStrLn $ printf @"You have %.2f dollars, %s" 3.62
-- ERROR: Call to printf missing argument fulfilling "%s"
-- Either provide an argument or rewrite the format string to not expect
-- one.

ghci> putStrLn $ printf @"You have %.2f dollars, %s" 3.62 "Luigi"
You have 3.62 dollars, Luigi

ghci> putStrLn $ printf @"You have %.2f dollars, %s" 3.62 "Luigi" 72
-- ERROR: An extra argument of type Integer was given to a call to printf
-- Either remove the argument, or rewrite the format string to include the
-- appropriate hole.
```

You can extend functionality with formatting for your own types by providing
instances of `FormatType`.

## Caveats

For medium-length or long strings, the parsing can be fairly slow and cause
slow compile times.  This might be due to the underlying mechanism that the
*[symbols][]* package exploits...or just GHC performance issues in general.

[symbols]: https://hackage.haskell.org/package/symbols

Moving to typechecker plugin based parsing *does* improve performance ...
however, I'm not sure how to get around requiring every module using `printf`
to require enabling the typechecker plugin, which isn't too great from a
usability standpoint.  Template Haskell based alternatives (like
*[th-printf][]*) already do require an extra pragma (for *QuasiQuotes*), though
so it might not be too bad in comparison.

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

*   Make faster
*   Tests
*   Support for localization/dynamic strings.  Should be possible, but we'd
    have to re-implement a subset of singletons.
