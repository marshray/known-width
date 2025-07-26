# known-width

Provides traits and const functions for integer types, implemented *only* for those primitive
integer types having a consistent width across all targets.

I.e., just:

- `u8`, `u16`, `u32`, `u64`, `u128`
- `i8`, `i16`, `i32`, `i64`, `i128`

### Example

```rust
use known_width::{ULte16, ULte32, into_u32, into_u64, wrapping_mul_u32, wrapping_add_u64};

/// Const multiply two up-to-16 bit unsigneds and add an up-to-32 bit unsigned.
/// Returns `u64`, no overflow.
const fn mul_add_u16_u16_u32<P: ULte16, Q: ULte16, N: ULte32>(p: P, q: Q, n: N) -> u64 {
    wrapping_add_u64(
        into_u64(wrapping_mul_u32(into_u32(p), into_u32(q))),
        into_u64(n) )
}

assert_eq!( mul_add_u16_u16_u32(128u8, 2u8, 1u16), 257);
assert_eq!( mul_add_u16_u16_u32(32768u16, 2u16, 1u32), 65537);
assert_eq!( mul_add_u16_u16_u32(u16::MAX, u16::MAX, u32::MAX), 8589803520);
```

## Motivation

There are other great crates that provide abstractions over numeric types. Two examples are:

- `num-traits`
  &nbsp; [\[crates.io\]](https://crates.io/crates/num-traits)
  &nbsp; [\[docs\]](https://docs.rs/num-traits/latest/num_traits/int/trait.PrimInt.html)
- `funty`
  &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; [\[crates.io\]](https://crates.io/crates/funty)
  &nbsp; [\[docs\]](https://docs.rs/funty/latest/funty/trait.Integral.html#foreign-impls)

`num-traits` in particular is the foundation of a widely-used numerics ecosystem. Those other
crates may be a better choice for many situations, or can be used in combination with this crate.

#### Why another integer traits crate?

Rust traits are used in [trait bounds](https://doc.rust-lang.org/stable/book/ch10-02-traits.html
), where they function as *constraints* to restrict the set of possible concrete types that
can be substituted for a generic `T`. (Traits also usually have methods you can call, but this
is a downstream effect.)

For example:

```rust
/// Sends the thing to that other thread.
/// Restricts `T` to only those types that `impl Send`.
fn send_thing<T: Send>(thing: T) {
    // ... code that relies on `thing` being `Send`-able ...
}
```

The primitive integer traits currently provided by other numeric crates come with impls not just for
Rust's [numeric integer types](https://doc.rust-lang.org/stable/reference/types/numeric.html#integer-types
), *they provide impls for the [machine-dependent integer types](
https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size
) [`usize`](https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size.usize
) and [`isize`](https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size.isize
) as well*. In many cases that is what you want.

But since `usize` and `isize` have machine-dependent widths, such traits are not sufficient to
ensure consistent cross-platform behavior. Very ordinary-looking code that builds without
warnings and passes all tests on a developer's 64-bit system may produce different results or
even panic when run on a 32- or 16-bit target.

Due to current limitations of Rust traits [(#143874)](
https://github.com/rust-lang/rust/issues/143874
) none of the functionality provided by those traits' methods is available in const contexts,
such as static initializers, where it may be needed the most.

## This crate

#### Traits

`known-width` provides a set of primitive integer traits which are specifically *not*
implemented for the machine-dependent types [`usize`] and [`isize`].

- Accept a single numeric integer type of known signedness and width:<br>
  `U8`, `U16`, `U32`, `U64`, `U128`<br>
  `I8`, `I16`, `I32`, `I64`, `I128`

- Accept any integer type of known signedness and at-least bits:<br>
  `UGte8`, `UGte16`, `UGte32`, `UGte64`, `UGte128`<br>
  `IGte8`, `IGte16`, `IGte32`, `IGte64`, `IGte128`

- Accept any integer type of known signedness and at-most bits:<br>
  `ULte8`, `ULte16`, `ULte32`, `ULte64`, `ULte128`<br>
  `ILte8`, `ILte16`, `ILte32`, `ILte64`, `ILte128`

[`usize`] and [`isize`] types only appear in this crate's definitions as the count operand
for shift and rotate operations, and in (`Try`) `From`/`Into` conversions.

#### const fns

A set of const fns are provided to convert the exact-width traits into their corresponding
concrete type:

```rust
    const fn get_u8<T: U8>(t: T) -> u8
    const fn get_u16<T: U16>(t: T) -> u16
    const fn get_u32<T: U32>(t: T) -> u32
    const fn get_u64<T: U64>(t: T) -> u64
    const fn get_u128<T: U128>(t: T) -> u128

    const fn get_i8<T: I8>(t: T) -> i8
    ...
    const fn get_i128<T: I128>(t: T) -> i128
```

In non-const contexts you can just use `.into()`.

Note that the function name has the intrinsic operation width or result type appended as
a suffix. If in the future stable Rust supports specialization or const trait methods, then
perhaps more ergonomic alternatives will be possible.

Const fns are also provided to convert the at-least-bits types into the concrete type.

```rust
    const fn into_u8<T: ULte8>(t: T) -> u8
    ...
    const fn into_u128<T: ULte128>(t: T) -> u128

    const fn into_i8<T: I8>(t: T) -> i8
    ...
    const fn into_i128<T: I128>(t: T) -> i128
```

Again, in most non-const contexts you can just use `.into()`.

There are also sets of `const fn`s which re-expose those defined as associated items
of the primitive types:

```rust
    const fn count_ones_u8<T: ULte8>(t: T) -> u32;
    const fn count_zeros_u64<T: U64>(t: T) -> u32;
    const fn cast_signed_u32<T: ULte32>(t: T) -> i32;
    const unsafe fn unchecked_sub_i16<T: I16, U: I16>(lhs: T, rhs: U) -> i16;
    const fn saturating_mul_u16<T: U16, U: U16 >(lhs: T, rhs: U) -> u16;
    const fn next_power_of_two_u64 <T: U64 >(t: T) -> u64;

    ... and more
```

The methods defined as traits are inherited.

The idea is to eventually relax the type bounds (e.g. `U16` to `ULte16`) when this
can be done without negatively affecting performance. In the absence of specialization or
const trait methods, this requires a runtime check which theoretically should be optimized
away, but this requires confirmation. This should not be a breaking change for user code.

### Toolchain support plan

- Use in [no-std] environments.
- May requre the [`rustversion`](https://docs.rs/rustversion/latest/rustversion/index.html)
  feature enabled for any functionality added to 'stable' Rust less than a year or two prior.

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you, as defined in the Apache-2.0
license, shall be dual licensed as above, without any additional terms
or conditions.
