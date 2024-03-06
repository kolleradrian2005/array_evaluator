# Array expression evaluator

This program takes in two array expressions from the user that determines whether they mean the same or not. For example `~A U ~A` is the same as `~(A ^ A)` according to De Morgan's Law.

## Usage

The operators available are

- Union - `U` _(infix)_
- Intersection - `^` _(infix)_
- Complementer - `~` _(prefix)_
- Asymmetric difference - `~` _(infix)_

## Build

```sh
cargo run --release
```

The output is in the `/target/release` folder
