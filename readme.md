## Core types: Integer, Natural, Positive

The primary module of the `integer-types` package is `Integer`, which exports
the following integer-like types:

Type          | Range
--------------|------------
`Integer`     | (-∞, ∞)
`Natural`     | (0, ∞)
`Positive`    | (1, ∞)

## The Signed type

In addition to `Integer`, there is also an equivalent type called `Signed` that
is represented as:

```haskell
data Signed = Zero | NonZero Sign Positive

data Sign = MinusSign | PlusSign
```

`Signed` also comes with bundled pattern synonyms that allow it to be used as if
it had the following definition:

```haskell
data Signed = Minus Positive | Zero | Plus Positive
```

## Monomorphic conversions

The following modules contain monomorphic conversion functions:

- `Integer.Integer`
- `Integer.Natural`
- `Integer.Positive`
- `Integer.Signed`

For example, you can convert from `Positive` to `Integer` using either
`Integer.Positive.toInteger` or `Integer.Integer.fromPositive`, which are two
names for the same function of type `Positive -> Integer`.

Since not all integers are positive, the corresponding function in the reverse
direction has a `Maybe` codomain. `Integer.Integer.toPositive` and
`Integer.Positive.fromInteger` have the type `Integer -> Maybe Positive`.

## Polymorphic conversions

The `Integer` module exports two polymorphic conversion functions. The first is
for conversions that always succeed, such as `Positive -> Integer`.

```haskell
convert :: IntegerConvert a b => a -> b
```

The second is for conversions that may fail because they convert to a subset of
the domain, such as `Integer -> Maybe Positive`.

```haskell
narrow :: IntegerNarrow a b => a -> Maybe b
```

## Finite integer subsets

In addition to the conversion utilities discussed above, this library also
provides some minimal support for converting to/from the `Word` and `Int` types.
These are system-dependent finite subsets of `Integer` that are sometimes used
for performance reasons.

```haskell
toFinite   :: (ConvertWithFinite a, Finite b) => a -> Maybe b
fromFinite :: (ConvertWithFinite a, Finite b) => b -> Maybe a
```

For example, `toFinite` may specialize as `Positive -> Maybe Int`, and
`fromFinite` may specialize as `Int -> Maybe Positive`.

## Monomorphic subtraction

For the `Integer` and `Signed` types that represent the full range of integers,
the standard arithmetic operations in the `Num` and `Integral` classes are
suitable.

For `Natural` and `Positive`, which are subsets of the integers, the standard
classes are not entirely appropriate. Consider, for example, subtraction.

```haskell
(-) :: Num a => a -> a -> a
```

`Natural` and `Positive` do belong to the `Num` class, but subtraction and some
other operations are partial; the expression `1 - 2` throws instead of returning
a value, because the integer result `-1` is negative and not representable by
either `Natural` or `Positive`.

For this reason, `Natural` and `Positive` have their own subtraction functions
that return `Signed`.

```haskell
-- from Integer.Positive
subtract :: Positive -> Positive -> Signed

-- from Integer.Natural
subtract :: Natural -> Natural -> Signed
```

## Polymorphic subtraction

In addition to the `(-)` method from the `Num` class and the `subtract`
functions for `Natural` and `Positive`, there are some polymorphic subtraction
functions in the `Integer` module. `subtractSigned` generalizes the two
monomorphic functions discussed in the previous section. Its codomain is
`Signed`.

```haskell
subtractSigned :: forall a. Subtraction a =>
    a -> a -> Signed
```

`subtractInteger` does the same thing, but gives the result as `Integer` instead
of `Signed`.

```haskell
subtractInteger :: forall a. Subtraction a =>
    a -> a -> Integer
```

The `subtract` function generalizes further. Its domain is any subtractable type
(`Natural`, `Positive`, `Integer`, or `Signed`) and its codomain is any type
that can represent the full range of integers (`Integer` or `Signed`).

```haskell
subtract :: forall b a. (Subtraction' b, Subtraction a) =>
    a -> a -> b
```
