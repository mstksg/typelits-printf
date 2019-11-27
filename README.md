# symbols-printf

Heavily inspired by *[Data.Symbol.Examples.Printf][symbols]*.

[symbols]: https://hackage.haskell.org/package/symbols-0.3.0.0/docs/Data-Symbol-Examples-Printf.html

```haskell
printf @"Hello %s, the year is %04y and you are %.2f meters tall" "Luigi" 2019 1.62
```

+-----------+------------------+--------------------+--------------------------+
| Method    | True Polyarity   | Naked Arguments    | Type feedback            |
+===========+==================+====================+==========================+
| `pprintf` | Yes              | No (requires `PP`) | Yes                      |
+-----------+------------------+--------------------+--------------------------+
| `rprintf` | No (HList-based) | Yes                | Yes                      |
+-----------+------------------+--------------------+--------------------------+
| `printf`  | Yes              | Yes                | No (Bad errors/guidance) |
+-----------+------------------+--------------------+--------------------------+
