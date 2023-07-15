## 0.1.4.0

Added module `Integer.AbsoluteDifference`

Added to the `Integer` module: `AbsoluteDifference (absoluteDifference)`

Date: 2023-07-15

## 0.1.3.0

Added modules `Integer.Increase`, `Integer.StrictlyIncrease`

Added classes to the `Integer` module:
`Increase (increase)`, `StrictlyIncrease (strictlyIncrease)`

Added to the `Integer.Integer` module: `increase`, `strictlyIncrease`

Added to the `Integer.Natural` module: `strictlyIncrease`

Added to the `Integer.Positive` module: `increase`

Added to the `Integer.Signed` module: `increase`, `strictlyIncrease`,
`one`, `addOne`, `subtractOne`

Date: 2023-07-14

## 0.1.2.0

Add `Read` instance for `Positive`

Date: 2023-06-26

## 0.1.1.0

Add `Hashable` instances for `Positive`, `Sign`, and `Signed`

Add `Enum` and `Bounded` instances for `Sign`

Date: 2023-04-22

## 0.1.0.0

Change type of `Integer.Natural.addOne` from
`Integer -> Integer` to `Natural -> Positive`

New functions:

```haskell
Integer.Natural.length :: [a] -> Natural
Integer.Positive.length :: NonEmpty a -> Positive
```

Date: 2023-02-09

## 0.0.0.1

Consolidate all the test suites into one

Remove `Safe` pragmas

Date: 2023-01-16

## 0.0.0.0

Initial release

Date: 2022-11-29
