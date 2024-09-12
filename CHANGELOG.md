Changelog
=========

Version 0.3.0.0
---------------

*September 11, 2024*

<https://github.com/mstksg/typelits-printf/releases/tag/v0.3.0.0>

*   Use `-XRequiredTypeArguments` in GHC 9.10. You can directly pass
    the `Symbol` without needing `@`:

    ```
    >>> putStrLn $ printf "You have %.2f dollars, %s" 3.62 "Luigi"
    You have 3.62 dollars, Luigi
    ```

    If you are on GHC 9.8 or lower, this is exported identically to
    `Text.Printf.printf`, which kind of looks identical, syntactically, if you
    use a string literal.

    If you still want the type-safe printf in GHC 9.8 or lower, use `printf'`
    for now.
*   Move to directly using `UnconsSymbol` and type-level `Char` literals from
    GHC, available only GHC 9.2+. This means we can support unicode in strings,
    and could be faster potentially.

Version 0.2.0.0
---------------

*February 26, 2020*

<https://github.com/mstksg/typelits-printf/releases/tag/v0.2.0.0>

*   Following <https://github.com/mstksg/typelits-printf/pull/3>, we can now
    get "all three" benefits with `printf`.  This version removes all traces
    of `pprintf`, `PP`, and `rprintf`, making `printf` the official
    one-size-fits-all function. Thanks @kcsongor!

Version 0.1.1.0
---------------

*February 26, 2020*

<https://github.com/mstksg/typelits-printf/releases/tag/v0.1.1.0>

*   Major bug fix in parser (thanks @kcsongor)

Version 0.1.0.0
---------------

*February 25, 2020*

<https://github.com/mstksg/typelits-printf/releases/tag/v0.1.0.0>

*   Initial release
