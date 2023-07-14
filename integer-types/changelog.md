## 0.1.3.0 (2023-07-14)

Added modules `Integer.Increase`, `Integer.StrictlyIncrease`

Added classes to the `Integer` module:
`Increase (increase)`, `StrictlyIncrease (strictlyIncrease)`

Added to the `Integer.Integer` module: `increase`, `strictlyIncrease`

Added to the `Integer.Natural` module: `strictlyIncrease`

Added to the `Integer.Positive` module: `increase`

Added to the `Integer.Signed` module: `increase`, `strictlyIncrease`,
`one`, `addOne`, `subtractOne`

## 0.1.2.0 (2023-06-26)

Add `Read` instance for `Positive`

## 0.1.1.0 (2023-04-22)

Add `Hashable` instances for `Positive`, `Sign`, and `Signed`

Add `Enum` and `Bounded` instances for `Sign`

## 0.1.0.0 (2023-02-09)

Change type of `Integer.Natural.addOne` from
`Integer -> Integer` to `Natural -> Positive`

New functions:

```haskell
Integer.Natural.length :: [a] -> Natural
Integer.Positive.length :: NonEmpty a -> Positive
```

## 0.0.0.1 (2023-01-16)

Consolidate all the test suites into one

Remove `Safe` pragmas

## 0.0.0.0 (2022-11-29)

Initial release
