// Copyright 2025 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![deny(elided_lifetimes_in_paths)]
#![deny(clippy::panic, clippy::unwrap_used, clippy::expect_used, clippy::manual_assert)]

//! Provides traits and const functions for integer types, implemented *only* for those primitive
//! integer types having a consistent width across all targets.
//!
//! I.e., just:
//!
//! - [`u8`], [`u16`], [`u32`], [`u64`], [`u128`]
//! - [`i8`], [`i16`], [`i32`], [`i64`], [`i128`].
//!
//! ### Example
//!
//! ```rust
//! use known_width::{ULte16, ULte32, into_u32, into_u64, wrapping_mul_u32, wrapping_add_u64};
//!
//! /// Const multiply two up-to-16 bit unsigneds and add an up-to-32 bit unsigned.
//! /// Returns `u64`, no overflow.
//! const fn mul_add_u16_u16_u32<P: ULte16, Q: ULte16, N: ULte32>(p: P, q: Q, n: N) -> u64 {
//!     wrapping_add_u64(
//!         into_u64(wrapping_mul_u32(into_u32(p), into_u32(q))),
//!         into_u64(n) )
//! }
//!
//! assert_eq!( mul_add_u16_u16_u32(128u8, 2u8, 1u16), 257);
//! assert_eq!( mul_add_u16_u16_u32(32768u16, 2u16, 1u32), 65537);
//! assert_eq!( mul_add_u16_u16_u32(u16::MAX, u16::MAX, u32::MAX), 8589803520);
//! ```
//!
//! ## Motivation
//!
//! There are other great crates that provide abstractions over numeric types. Two examples are:
//!
//! - `num-traits`
//!   &nbsp; [\[crates.io\]](https://crates.io/crates/num-traits)
//!   &nbsp; [\[docs\]](https://docs.rs/num-traits/latest/num_traits/int/trait.PrimInt.html)
//! - `funty`
//!   &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; [\[crates.io\]](https://crates.io/crates/funty)
//!   &nbsp; [\[docs\]](https://docs.rs/funty/latest/funty/trait.Integral.html#foreign-impls)
//!
//! `num-traits` in particular is the foundation of a widely-used numerics ecosystem. Those other
//! crates may be a better choice for many situations, or can be used in combination with this crate.
//!
//! #### Why another integer traits crate?
//!
//! Rust traits are used in [trait bounds](https://doc.rust-lang.org/stable/book/ch10-02-traits.html
//! ), where they function as *constraints* to restrict the set of possible concrete types that
//! can be substituted for a generic `T`. (Traits also usually have methods you can call, but this
//! is a downstream effect.)
//!
//! For example:
//!
//! ```rust
//! /// Sends the thing to that other thread.
//! /// Restricts `T` to only those types that `impl Send`.
//! fn send_thing<T: Send>(thing: T) {
//!     // ... code that relies on `thing` being `Send`-able ...
//! }
//! ```
//!
//! The primitive integer traits currently provided by other numeric crates come with impls not just for
//! Rust's [numeric integer types](https://doc.rust-lang.org/stable/reference/types/numeric.html#integer-types
//! ), *they provide impls for the [machine-dependent integer types](
//! https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size
//! ) [`usize`](https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size.usize
//! ) and [`isize`](https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size.isize
//! ) as well*. In many cases that is what you want.
//!
//! But since `usize` and `isize` have machine-dependent widths, such traits are not sufficient to
//! ensure consistent cross-platform behavior. Very ordinary-looking code that builds without
//! warnings and passes all tests on a developer's 64-bit system may produce different results or
//! even panic when run on a 32- or 16-bit target.
//!
//! Due to current limitations of Rust traits [(#143874)](
//! https://github.com/rust-lang/rust/issues/143874
//! ) none of the functionality provided by those traits' methods is available in const contexts,
//! such as static initializers, where it may be needed the most.
//!
//! ## This crate
//!
//! #### Traits
//!
//! `known-width` provides a set of primitive integer traits which are specifically *not*
//! implemented for the machine-dependent types [`usize`] and [`isize`].
//!
//! - Accept a single numeric integer type of known signedness and width:<br>
//!   `U8`, `U16`, `U32`, `U64`, `U128`<br>
//!   `I8`, `I16`, `I32`, `I64`, `I128`
//!
//! - Accept any integer type of known signedness and at-least bits:<br>
//!   `UGte8`, `UGte16`, `UGte32`, `UGte64`, `UGte128`<br>
//!   `IGte8`, `IGte16`, `IGte32`, `IGte64`, `IGte128`
//!
//! - Accept any integer type of known signedness and at-most bits:<br>
//!   `ULte8`, `ULte16`, `ULte32`, `ULte64`, `ULte128`<br>
//!   `ILte8`, `ILte16`, `ILte32`, `ILte64`, `ILte128`
//!
//! [`usize`] and [`isize`] types only appear in this crate's definitions as the count operand
//! for shift and rotate operations, and in (`Try`) `From`/`Into` conversions.
//!
//! #### const fns
//!
//! A set of const fns are provided to convert the exact-width traits into their corresponding
//! concrete type:
//!
//! ```rust
//! # #[allow(rustdoc::invalid_rust_codeblocks)]
//! # /*
//!     const fn get_u8<T: U8>(t: T) -> u8
//!     const fn get_u16<T: U16>(t: T) -> u16
//!     const fn get_u32<T: U32>(t: T) -> u32
//!     const fn get_u64<T: U64>(t: T) -> u64
//!     const fn get_u128<T: U128>(t: T) -> u128
//!
//!     const fn get_i8<T: I8>(t: T) -> i8
//!     ...
//!     const fn get_i128<T: I128>(t: T) -> i128
//! # */
//! ```
//!
//! In non-const contexts you can just use `.into()`.
//!
//! Note that the function name has the intrinsic operation width or result type appended as
//! a suffix. If in the future stable Rust supports specialization or const trait methods, then
//! perhaps more ergonomic alternatives will be possible.
//!
//! Const fns are also provided to convert the at-least-bits types into the concrete type.
//!
//! ```rust
//! # #[allow(rustdoc::invalid_rust_codeblocks)]  /*
//!     const fn into_u8<T: ULte8>(t: T) -> u8
//!     ...
//!     const fn into_u128<T: ULte128>(t: T) -> u128
//!
//!     const fn into_i8<T: I8>(t: T) -> i8
//!     ...
//!     const fn into_i128<T: I128>(t: T) -> i128
//! # */
//! ```
//!
//! Again, in most non-const contexts you can just use `.into()`.
//!
//! There are also sets of `const fn`s which re-expose those defined as associated items
//! of the primitive types:
//!
//! ```rust
//! # #[allow(rustdoc::invalid_rust_codeblocks)]  /*
//!     const fn count_ones_u8<T: ULte8>(t: T) -> u32;
//!     const fn count_zeros_u64<T: U64>(t: T) -> u32;
//!     const fn cast_signed_u32<T: ULte32>(t: T) -> i32;
//!     const unsafe fn unchecked_sub_i16<T: I16, U: I16>(lhs: T, rhs: U) -> i16;
//!     const fn saturating_mul_u16<T: U16, U: U16 >(lhs: T, rhs: U) -> u16;
//!     const fn next_power_of_two_u64 <T: U64 >(t: T) -> u64;
//!
//!     ... and more
//! # */
//! ```
//!
//! The methods defined as traits are inherited.
//!
//! The idea is to eventually relax the type bounds (e.g. `U16` to `ULte16`) when this
//! can be done without negatively affecting performance. In the absence of specialization or
//! const trait methods, this requires a runtime check which theoretically should be optimized
//! away, but this requires confirmation. This should not be a breaking change for user code.
//!
//! ### Toolchain support plan
//!
//! - Use in [no-std] environments.
//! - May requre the [`rustversion`](https://docs.rs/rustversion/latest/rustversion/index.html)
//!   feature enabled for any functionality added to 'stable' Rust less than a year or two prior.
//!
//! Notable Rust stable releases:
//!
//! - [1.88](https://github.com/rust-lang/rust/blob/master/RELEASES.md#version-1880-2025-06-26
//!   ) 2025-06-26 &nbsp; The current stable at the time of this crate version.
//! - [1.80](https://github.com/rust-lang/rust/blob/master/RELEASES.md#version-1800-2024-07-25
//!   ) 2024-07-25 &nbsp; The current stable release one year prior to this crate version.
//! - [1.79](https://github.com/rust-lang/rust/blob/master/RELEASES.md#version-1790-2024-06-13
//!   ) 2024-06-13 &nbsp; Stabilized:
//!   - [`::core::num::NonZero`]
//!   - [`{primitive integer}::unchecked_add()`](u8::unchecked_add)
//!   - [`{primitive integer}::unchecked_mul()`](u8::unchecked_mul)
//!   - [`{primitive integer}::unchecked_sub()`](u8::unchecked_sub)

#![no_std]

#![allow(non_camel_case_types)]

// Utility macro, invokes the supplied macro with $types_info:tt.
#[rustfmt::skip]
macro_rules! with_types_info {
    // Main entry
    { $name:ident { $($args:tt)* } } => {
        // Invoke ourselves again, but with $has_core_num_NonZero.
        with_has_core_num_NonZero! {
            with_types_info { @step2 $name { $($args)* } }
        }
    };

    { @step2 $name:ident { $($args:tt)* } $has_core_num_NonZero:tt } => {
        // Invoke ourselves again, but with $has_core_num_NonZero.
        with_has_num_traits! {
            with_types_info {
                @step3
                $name { $($args)* }
                { has_core_num_NonZero: $has_core_num_NonZero }
            }
        }
    };

    { @step3
        $name:ident { $($args:tt)* }
        { has_core_num_NonZero: $has_core_num_NonZero:tt }
        $has_num_traits:ident
    } => {
        $name! {
            $($args)*
            {
                KWPIU: [
                    ( u8,     8, 3,   t_u8, ),
                    ( u16,   16, 4,  t_u16, ),
                    ( u32,   32, 5,  t_u32, ),
                    ( u64,   64, 6,  t_u64, ),
                    ( u128, 128, 7, t_u128, )
                ],
                KWPIS: [
                    ( i8,     8, 3,   t_i8, ),
                    ( i16,   16, 4,  t_i16, ),
                    ( i32,   32, 5,  t_i32, ),
                    ( i64,   64, 6,  t_i64, ),
                    ( i128, 128, 7, t_i128, )
                ],
                has_core_num_NonZero: $has_core_num_NonZero,
                has_num_traits: $has_num_traits
            }
        }
    };
}

// Invokes the supplied macro with `yes` or `no` indicating whether
// whether the [`::core::num::NonZero`] type is known to be supported.

#[cfg(not(feature="rustversion"))]
macro_rules! with_has_core_num_NonZero {
    { $name:ident { $($args:tt)* } } => {
        $name! { $($args)* no }
    };
}

#[cfg(feature="rustversion")] #[rustversion::not(since(1.79))]
macro_rules! with_has_core_num_NonZero {
    { $name:ident { $($args:tt)* } } => {
        $name! { $($args)* no }
    };
}

#[cfg(feature="rustversion")] #[rustversion::since(1.79)]
macro_rules! with_has_core_num_NonZero {
    { $name:ident { $($args:tt)* } } => {
        $name! { $($args)* yes }
    };
}

// Invokes the supplied macro with `yes` or `no` indicating whether
// the `num-traits` feature is enabled.

#[cfg(not(feature="num-traits"))]
macro_rules! with_has_num_traits {
    { $name:ident { $($args:tt)* } } => {
        $name! { $($args)* no }
    };
}

#[cfg(feature="num-traits")]
macro_rules! with_has_num_traits {
    { $name:ident { $($args:tt)* } } => {
        $name! { $($args)* yes }
    };
}

// The main macro which expands into the content.
#[rustfmt::skip]
macro_rules! m {
    // Main entry
    { } => {
        // Invoke ourselves again, but with $types_info.
        with_types_info! { m { @emit_file } }
    };

    { @emit_file $types_info:tt } => {
        m! { @traits $types_info }
        m! { @trait_impls $types_info $types_info }
    };

    //============================================================== define traits

    { @traits $types_info:tt } => {
        m! { @traits2
            $types_info
            {
                // Traits implemented on Self
                // no Rhs, no Output
                Self_traits_NoRhs_NoOutput: [
                    ( 'static )
                    ( Sized )
                    ( Default )
                    ( Clone )
                    ( Copy )
                    ( Send )
                    ( Sync )
                    ( Unpin )
                    ( ::core::cmp::Eq )
                    ( ::core::cmp::Ord )
                    ( ::core::fmt::Debug )
                    ( ::core::fmt::Display )
                    ( ::core::fmt::LowerExp )
                    ( ::core::fmt::UpperExp )
                    ( ::core::fmt::Octal )
                    ( ::core::fmt::LowerHex )
                    ( ::core::fmt::UpperHex )
                    ( ::core::hash::Hash )
                    ( ::core::panic::UnwindSafe )
                    ( ::core::panic::RefUnwindSafe )
                    ( From<bool> )
                    ( TryFrom<u8> )
                    ( TryFrom<u16> )
                    ( TryFrom<u32> )
                    ( TryFrom<u64> )
                    ( TryFrom<u128> )
                    ( TryFrom<usize> )
                    ( TryFrom<i8> )
                    ( TryFrom<i16> )
                    ( TryFrom<i32> )
                    ( TryFrom<i64> )
                    ( TryFrom<i128> )
                    ( TryFrom<isize> )
                    ( TryInto<u8> )
                    ( TryInto<u16> )
                    ( TryInto<u32> )
                    ( TryInto<u64> )
                    ( TryInto<u128> )
                    ( TryInto<usize> )
                    ( TryInto<i8> )
                    ( TryInto<i16> )
                    ( TryInto<i32> )
                    ( TryInto<i64> )
                    ( TryInto<i128> )
                    ( TryInto<isize> )
                    ( ::core::str::FromStr)
                ],

                // Traits implemented on Self
                // with an Rhs of Self and &Self
                // no Output
                Self_traits_RhsSelf_NoOutput: [
                    ( ::core::cmp::PartialEq )
                    ( ::core::cmp::PartialOrd )
                ],

                // Traits implemented on Self and &Self
                // no Rhs
                // with Output of Self
                SelfRef_traits_NoRhs_OutputSelf: [
                    ( ::core::ops::Not )
                ],
                // Traits implemented on Self
                // with an Rhs of Self and &Self
                // no Output
                Self_traits_RhsSelfRef_NoOutput: [
                    ( ::core::ops::AddAssign )
                    ( ::core::ops::BitAndAssign )
                    ( ::core::ops::BitOrAssign )
                    ( ::core::ops::BitXorAssign )
                    ( ::core::ops::DivAssign )
                    ( ::core::ops::MulAssign )
                    ( ::core::ops::RemAssign )
                    ( ::core::ops::SubAssign )
                    ( ::core::iter::Sum )
                    ( ::core::iter::Product )
                ],
                // Traits implemented on Self and &Self
                // with an Rhs of Self and &Self
                // with Output of Self
                SelfRef_traits_RhsSelfRef_OutputSelf: [
                    ( ::core::ops::Add )
                    ( ::core::ops::BitAnd )
                    ( ::core::ops::BitOr )
                    ( ::core::ops::BitXor )
                    ( ::core::ops::Div )
                    ( ::core::ops::Mul )
                    ( ::core::ops::Rem )
                    ( ::core::ops::Sub )
                ],

                // Traits implemented on Self
                // with a specific Rhs also &Rhs
                // no Output
                Self_traits_specificRhsAndRef_NoOutput: [
                    ( ( ::core::ops::ShlAssign ) u8 )
                    ( ( ::core::ops::ShlAssign ) u16 )
                    ( ( ::core::ops::ShlAssign ) u32 )
                    ( ( ::core::ops::ShlAssign ) u64 )
                    ( ( ::core::ops::ShlAssign ) u128 )
                    ( ( ::core::ops::ShlAssign ) usize )
                    ( ( ::core::ops::ShlAssign ) i8 )
                    ( ( ::core::ops::ShlAssign ) i16 )
                    ( ( ::core::ops::ShlAssign ) i32 )
                    ( ( ::core::ops::ShlAssign ) i64 )
                    ( ( ::core::ops::ShlAssign ) i128 )
                    ( ( ::core::ops::ShlAssign ) isize )

                    ( ( ::core::ops::ShrAssign ) u8 )
                    ( ( ::core::ops::ShrAssign ) u16 )
                    ( ( ::core::ops::ShrAssign ) u32 )
                    ( ( ::core::ops::ShrAssign ) u64 )
                    ( ( ::core::ops::ShrAssign ) u128 )
                    ( ( ::core::ops::ShrAssign ) usize )
                    ( ( ::core::ops::ShrAssign ) i8 )
                    ( ( ::core::ops::ShrAssign ) i16 )
                    ( ( ::core::ops::ShrAssign ) i32 )
                    ( ( ::core::ops::ShrAssign ) i64 )
                    ( ( ::core::ops::ShrAssign ) i128 )
                    ( ( ::core::ops::ShrAssign ) isize )
                ],

                // Traits implemented on Self and &Self
                // with a specific Rhs also &Rhs
                // with Output of Self
                SelfRef_traits_specificRhsAndRef_OutputSelf: [
                    ( ( ::core::ops::Shl ) u8 )
                    ( ( ::core::ops::Shl ) u16 )
                    ( ( ::core::ops::Shl ) u32 )
                    ( ( ::core::ops::Shl ) u64 )
                    ( ( ::core::ops::Shl ) u128 )
                    ( ( ::core::ops::Shl ) usize )
                    ( ( ::core::ops::Shl ) i8 )
                    ( ( ::core::ops::Shl ) i16 )
                    ( ( ::core::ops::Shl ) i32 )
                    ( ( ::core::ops::Shl ) i64 )
                    ( ( ::core::ops::Shl ) i128 )
                    ( ( ::core::ops::Shl ) isize )

                    ( ( ::core::ops::Shr ) u8 )
                    ( ( ::core::ops::Shr ) u16 )
                    ( ( ::core::ops::Shr ) u32 )
                    ( ( ::core::ops::Shr ) u64 )
                    ( ( ::core::ops::Shr ) u128 )
                    ( ( ::core::ops::Shr ) usize )
                    ( ( ::core::ops::Shr ) i8 )
                    ( ( ::core::ops::Shr ) i16 )
                    ( ( ::core::ops::Shr ) i32 )
                    ( ( ::core::ops::Shr ) i64 )
                    ( ( ::core::ops::Shr ) i128 )
                    ( ( ::core::ops::Shr ) isize )
                ],
            }
        }
    };

    { @traits2 $types_info:tt $kwpi_supers:tt } => {
        m! { @trait_Integer $types_info $types_info $kwpi_supers }
        m! { @trait_Unsigned $types_info }
        m! { @trait_SignedInteger $types_info }
        m! { @traits_Exactly $types_info }
    };

    { @trait_Integer $types_info:tt
        {
            KWPIU: $kwpiu:tt,
            KWPIS: $kwpis:tt
            $( , $extra_k:ident : $extra_v:tt )*
        }
        {
            Self_traits_NoRhs_NoOutput:           [ $( ( $($Self_traits_NoRhs_NoOutput:tt)+ ) )* ],
            Self_traits_RhsSelf_NoOutput:         [ $( ( $($Self_traits_RhsSelf_NoOutput:tt)+ ) )* ],
            SelfRef_traits_NoRhs_OutputSelf:      [ $( ( $($SelfRef_traits_NoRhs_OutputSelf:tt)+ ) )* ],
            Self_traits_RhsSelfRef_NoOutput:      [ $( ( $($Self_traits_RhsSelfRef_NoOutput:tt)+ ) )* ],
            SelfRef_traits_RhsSelfRef_OutputSelf: [ $( ( $($SelfRef_traits_RhsSelfRef_OutputSelf:tt)+ ) )* ],
            Self_traits_specificRhsAndRef_NoOutput: [
                $( ( ( $($Self_specificRhsAndRef:tt)+ ) $Self_specificRhs:ty ) )*
            ],
            SelfRef_traits_specificRhsAndRef_OutputSelf: [
                $( ( ( $($SelfRef_specificRhsAndRef_OutputSelf:tt)+ ) $SelfRef_specificRhs:ty ) )*
            ],
        }
    } => {
        m! { @trait_KWPI2
            Supertraits: [
                $( ( $( $Self_traits_NoRhs_NoOutput )+ ) )*

                $( ( $( $Self_traits_RhsSelf_NoOutput )+ <Self> ) )*

                $( ( $( $SelfRef_traits_NoRhs_OutputSelf )+ <Output=Self> ) )*

                $( ( $( $Self_traits_RhsSelfRef_NoOutput )+ <Self> )
                   ( for<'a> $( $Self_traits_RhsSelfRef_NoOutput )+ <&'a Self> ) )*

                $( ( $( $SelfRef_traits_RhsSelfRef_OutputSelf )+ <Self, Output=Self> )
                   ( for<'a> $( $SelfRef_traits_RhsSelfRef_OutputSelf )+ <&'a Self, Output=Self> ) )*

                $( ( $($Self_specificRhsAndRef)+ <$Self_specificRhs> )
                   ( for<'a> $($Self_specificRhsAndRef)+ <&'a $Self_specificRhs> ) )*

                $( ( $($SelfRef_specificRhsAndRef_OutputSelf)+ <$SelfRef_specificRhs, Output=Self> )
                   ( for<'a> $($SelfRef_specificRhsAndRef_OutputSelf)+ <&'a $SelfRef_specificRhs, Output=Self>  ) )*
            ]
        }
    };

    { @trait_KWPI2
        Supertraits: [ $( ( $($supertraits:tt)+ ) )* ]
    } => {
        /// One of the [numeric integer types](
        /// https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int )
        /// having a consistent width across all platforms:
        /// `u8`, `u16`, `u32`, `u64`, `u128`, `i8`, `i16`, `i32`, `i64`, or `i128`.
        ///
        /// This does not include [`usize`](
        /// https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size.usize),
        /// or [`isize`](
        /// https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size.isize),
        /// which are ["Machine-dependent integer types"](
        /// https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size).
        pub trait Integer:
            $( $($supertraits)+ + )*
        {
            /// The base type.
            type Type;

            /// The base-2 log of the width of the base type in bits.
            const BITS_LOG2: u16;

            // The std primitive types mostly seem to implement bit counts as `u32`.
            // Most types implement [`std::ops::ShlAssign<T>`] for all numeric integer types,
            // execept for [`std::num::Wrapping`] which only implements it for [`usize`].
            //
            /// The width of the base type in bits.
            const BITS: u32;

            /// The largest value that the base type can represent.
            const MAX: Self::Type;

            /// The largest value that the base type can represent.
            const MIN: Self::Type;

            /// The value `0`.
            const ZERO: Self::Type;
        }
    };

    { @trait_Unsigned $types_info:tt } => {
        m! { @traits_UGte }
        m! { @traits_Unsigned_AtMost }

        /// One of the [unsigned integer types](
        /// https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.unsigned )
        /// having a consistent width across all platforms: `u8`, `u16`, `u32`, `u64`, or `u128`.
        ///
        /// This does not include [`usize`](
        /// https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size.usize),
        /// which is considered a ["Machine-dependent integer type"](
        /// https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size).
        pub trait UnsignedInteger :
            Integer +
            From<u8> +
            Into<u128>
        { }
    };

    { @traits_UGte } => {
        /// `u8`, `u16`, `u32`, `u64`, or `u128`
        pub trait UGte8 :
            UnsignedInteger +
            From<u8>
        { }

        /// `u16`, `u32`, `u64`, or `u128`
        pub trait UGte16 :
            UnsignedInteger +
            UGte8 +
            From<u16>
        { }

        /// `u32`, `u64`, or `u128`
        pub trait UGte32 :
            UnsignedInteger +
            UGte16 +
            From<u32> +
            From<char>
        { }

        /// `u64` or `u128`
        pub trait UGte64 :
            UnsignedInteger +
            UGte32 +
            From<u64>
        { }

        /// `u128`
        pub trait UGte128 :
            UnsignedInteger +
            UGte64 +
            From<u128>
        { }
    };

    { @traits_Unsigned_AtMost } => {
        /// `u8`, `u16`, `u32`, `u64`, or `u128`
        pub trait ULte128 :
            UnsignedInteger +
            Into<u128>
        { }

        #[inline(always)] pub const fn into_u128<T: ULte128>(t: T) -> u128 {
            match T::BITS {
                8   => unsafe { ::core::mem::transmute_copy::<_, u8>(&t) as u128 },
                16  => unsafe { ::core::mem::transmute_copy::<_, u16>(&t) as u128 },
                32  => unsafe { ::core::mem::transmute_copy::<_, u32>(&t) as u128 },
                64  => unsafe { ::core::mem::transmute_copy::<_, u64>(&t) as u128 },
                128 => unsafe { ::core::mem::transmute_copy::<_, u128>(&t) },
                _ => unreachable!(),
            }
        }

        /// `u8`, `u16`, `u32`, or `u64`
        pub trait ULte64 :
            UnsignedInteger +
            ULte128 +
            Into<u64> +
            Into<i128>
        { }

        #[inline(always)] pub const fn into_u64<T: ULte64>(t: T) -> u64 {
            match T::BITS {
                8   => unsafe { ::core::mem::transmute_copy::<_, u8>(&t) as u64 },
                16  => unsafe { ::core::mem::transmute_copy::<_, u16>(&t) as u64 },
                32  => unsafe { ::core::mem::transmute_copy::<_, u32>(&t) as u64 },
                64  => unsafe { ::core::mem::transmute_copy::<_, u64>(&t) },
                _ => unreachable!(),
            }
        }

        /// `u8`, `u16`, or `u32`
        pub trait ULte32 :
            UnsignedInteger +
            ULte64 +
            Into<u32> +
            Into<i64>
        { }

        #[inline(always)] pub const fn into_u32<T: ULte32>(t: T) -> u32 {
            match T::BITS {
                8  => unsafe { ::core::mem::transmute_copy::<_, u8>(&t) as u32 },
                16 => unsafe { ::core::mem::transmute_copy::<_, u16>(&t) as u32 },
                32 => unsafe { ::core::mem::transmute_copy::<_, u32>(&t) },
                _ => unreachable!(),
            }
        }

        /// `u8` or `u16`
        pub trait ULte16 :
            UnsignedInteger +
            ULte32 +
            ::core::convert::TryFrom<char> +
            Into<u16> +
            Into<i32> +
            Into<usize>
        { }

        #[inline(always)] pub const fn into_u16<T: ULte16>(t: T) -> u16 {
            match T::BITS {
                8  => unsafe { ::core::mem::transmute_copy::<_, u8>(&t) as u16 },
                16 => unsafe { ::core::mem::transmute_copy::<_, u16>(&t) },
                _ => unreachable!(),
            }
        }

        /// `u8`
        pub trait ULte8 :
            UnsignedInteger +
            ULte16 +
            Into<u8> +
            Into<i16> +
            Into<isize> +
            Into<usize>
        { }

        #[inline(always)] pub const fn into_u8<T: ULte8>(t: T) -> u8 {
            match T::BITS {
                8 => unsafe { ::core::mem::transmute_copy::<_, u8>(&t) }
                _ => unreachable!(),
            }
        }
    };

    { @trait_SignedInteger $types_info:tt } => {
        /// One of the [signed integer types](
        /// https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.signed )
        /// having a consistent width across all platforms: `i8`, `i16`, `i32`, `i64`, or `i128`.
        ///
        /// This does not include [`isize`](
        /// https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size.isize),
        /// which is considered a ["Machine-dependent integer type"](
        /// https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size).
        pub trait SignedInteger :
            Integer +
            From<i8> +
            Into<i128> +
            ::core::ops::Neg
        { }

        m! { @traits_IGte }
        m! { @traits_Signed_AtMost }
    };

    { @traits_IGte } => {
        /// `i8`, `i16`, `i32`, `i64`, or `i128`
        pub trait IGte8 :
            SignedInteger +
            From<i8>
        { }

        /// `i16`, `i32`, `i64`, or `i128`
        pub trait IGte16 :
            SignedInteger +
            IGte8 +
            From<i16> +
            From<u8>
        { }

        /// `i32`, `i64`, or `i128`
        pub trait IGte32 :
            SignedInteger +
            IGte16 +
            From<i32> +
            From<u16>
        { }

        /// `i64` or `i128`
        pub trait IGte64 :
            SignedInteger +
            IGte32 +
            From<i64> +
            From<u32>
        { }

        /// `i128`
        pub trait IGte128 :
            SignedInteger +
            IGte64 +
            From<i128> +
            From<u64>
        { }
    };

    { @traits_Signed_AtMost } => {
        /// `i8`, `i16`, `i32`, `i64`, or `i128`
        pub trait ILte128 :
            SignedInteger +
            Into<i128>
        { }

        #[inline(always)] pub const fn into_i128<T: ILte128>(t: T) -> i128 {
            match T::BITS {
                8   => unsafe { ::core::mem::transmute_copy::<_, i8>(&t) as i128 },
                16  => unsafe { ::core::mem::transmute_copy::<_, i16>(&t) as i128 },
                32  => unsafe { ::core::mem::transmute_copy::<_, i32>(&t) as i128 },
                64  => unsafe { ::core::mem::transmute_copy::<_, i64>(&t) as i128 },
                128 => unsafe { ::core::mem::transmute_copy::<_, i128>(&t) },
                _ => unreachable!(),
            }
        }

        /// `i8`, `i16`, `i32`, or `i64`
        pub trait ILte64 :
            SignedInteger +
            ILte128 +
            Into<i64>
        { }

        #[inline(always)] pub const fn into_i64<T: ILte64>(t: T) -> i64 {
            match T::BITS {
                8   => unsafe { ::core::mem::transmute_copy::<_, i8>(&t) as i64 },
                16  => unsafe { ::core::mem::transmute_copy::<_, i16>(&t) as i64 },
                32  => unsafe { ::core::mem::transmute_copy::<_, i32>(&t) as i64 },
                64  => unsafe { ::core::mem::transmute_copy::<_, i64>(&t) },
                _ => unreachable!(),
            }
        }

        /// `i8`, `i16`, or `i32`
        pub trait ILte32 :
            SignedInteger +
            ILte64 +
            Into<i32>
        { }

        #[inline(always)] pub const fn into_i32<T: ILte32>(t: T) -> i32 {
            match T::BITS {
                8   => unsafe { ::core::mem::transmute_copy::<_, i8>(&t) as i32 },
                16  => unsafe { ::core::mem::transmute_copy::<_, i16>(&t) as i32 },
                32  => unsafe { ::core::mem::transmute_copy::<_, i32>(&t) },
                _ => unreachable!(),
            }
        }

        /// `i8` or `i16`
        pub trait ILte16 :
            SignedInteger +
            ILte32 +
            Into<i16> +
            Into<isize>
        { }

        #[inline(always)] pub const fn into_i16<T: ILte16>(t: T) -> i16 {
            match T::BITS {
                8   => unsafe { ::core::mem::transmute_copy::<_, i8>(&t) as i16 },
                16  => unsafe { ::core::mem::transmute_copy::<_, i16>(&t) },
                _ => unreachable!(),
            }
        }

        /// `i8`
        pub trait ILte8 :
            SignedInteger +
            ILte16 +
            Into<i8>
        { }

        #[inline(always)] pub const fn into_i8<T: ILte8>(t: T) -> i8 {
            match T::BITS {
                8   => unsafe { ::core::mem::transmute_copy::<_, i8>(&t) },
                _ => unreachable!(),
            }
        }
    };

    { @traits_Exactly $types_info:tt } => {

        //--------- Unsigned

        m! { @traits_Exactly_step2 $types_info u8 [
            (UnsignedInteger)
            (UGte8)
            (ULte8)
            (Into<char>) ] }

        m! { @traits_Exactly_step2 $types_info u16 [
            (UnsignedInteger)
            (UGte16)
            (ULte16) ] }

        m! { @traits_Exactly_step2 $types_info u32 [
            (UnsignedInteger)
            (UGte32)
            (ULte32)
            (TryInto<char>) ] }

        m! { @traits_Exactly_step2 $types_info u64 [
            (UnsignedInteger)
            (UGte64)
            (ULte64) ] }

        m! { @traits_Exactly_step2 $types_info u128 [
            (UnsignedInteger)
            (UGte128)
            (ULte128) ] }

        //--------- Signed

        m! { @traits_Exactly_step2 $types_info i8 [
            (SignedInteger)
            (IGte8)
            (ILte8) ] }

        m! { @traits_Exactly_step2 $types_info i16 [
            (SignedInteger)
            (IGte16)
            (ILte16) ] }

        m! { @traits_Exactly_step2 $types_info i32 [
            (SignedInteger)
            (IGte32)
            (ILte32) ] }

        m! { @traits_Exactly_step2 $types_info i64 [
            (SignedInteger)
            (IGte64)
            (ILte64) ] }

        m! { @traits_Exactly_step2 $types_info i128 [
            (SignedInteger)
            (IGte128)
            (ILte128) ] }
    };

    { @traits_Exactly_step2
        {
            KWPIU: $kwpiu:tt,
            KWPIS: $kwpis:tt,
            has_core_num_NonZero: $has_core_num_NonZero:tt,
            has_num_traits: $has_num_traits:ident
            $( , $extra_k:ident : $extra_v:tt )*
        }
        $type_name:ident
        $prereqs:tt
    } => {
        m! { @traits_Exactly_step3 $has_core_num_NonZero $type_name $prereqs }
    };

    // Maybe add `From` and `TryInto` traits for `::core::num::Nonzero` to
    // the list of prereqs.
    { @traits_Exactly_step3
        yes // <------------------------------------------ has_core_num_NonZero == 'yes'
        $type_name:ident
        [ $( ( $($prereqs:tt)+ ) )* ]
    } => {
        m! { @traits_Exactly_final $type_name [
            $( ( $($prereqs)+ ) )*
            ( From<::core::num::NonZero< $type_name >> )
            ( TryInto<::core::num::NonZero< $type_name >> )
            ( ::core::ops::BitOr<::core::num::NonZero<$type_name>> )
        ] [
            ( ::core::ops::DivAssign<::core::num::NonZero<$type_name>> ) // Unsigned only
            ( ::core::ops::RemAssign<::core::num::NonZero<$type_name>> )
            ( ::core::ops::Div<::core::num::NonZero<$type_name>> )
            ( ::core::ops::Rem<::core::num::NonZero<$type_name>> )
        ] }
    };
    { @traits_Exactly_step3
        $has_core_num_NonZero:tt // <--------------------- has_core_num_NonZero != 'yes'
        $type_name:ident
        $prereqs:tt
    } => {
        m! { @traits_Exactly_final $type_name $prereqs [ ] }
    };

    //-------------------- Specializations that actually generate the `Exactly` traits.

    //---------- Unsigned

    { @traits_Exactly_final u8 [ $( ( $($prereqs:tt)+ ) )* ] [ $( ( $($prereqs_unsigned:tt)+ ) )* ] } => {
        /// - `u8`
        pub trait U8 :
            $( $($prereqs)+ + )*
        { }
        m! { @traits_Exactly_get U8 u8 get_u8 }
    };

    { @traits_Exactly_final u16 [ $( ( $($prereqs:tt)+ ) )* ] [ $( ( $($prereqs_unsigned:tt)+ ) )* ] } => {
        /// - `u16`
        pub trait U16 :
            $( $($prereqs)+ + )*
        { }
        m! { @traits_Exactly_get U16 u16 get_u16 }
    };

    { @traits_Exactly_final u32 [ $( ( $($prereqs:tt)+ ) )* ] [ $( ( $($prereqs_unsigned:tt)+ ) )* ] } => {
        /// - `u32`
        pub trait U32 :
            $( $($prereqs)+ + )*
        { }
        m! { @traits_Exactly_get U32 u32 get_u32 }
    };

    { @traits_Exactly_final u64 [ $( ( $($prereqs:tt)+ ) )* ] [ $( ( $($prereqs_unsigned:tt)+ ) )* ] } => {
        /// - `u64`
        pub trait U64 :
            $( $($prereqs)+ + )*
        { }
        m! { @traits_Exactly_get U64 u64 get_u64 }
    };

    { @traits_Exactly_final u128 [ $( ( $($prereqs:tt)+ ) )* ] [ $( ( $($prereqs_unsigned:tt)+ ) )* ] } => {
        /// - `u128`
        pub trait U128 :
            $( $($prereqs)+ + )*
        { }
        m! { @traits_Exactly_get U128 u128 get_u128 }
    };

    //---------- Signed

    { @traits_Exactly_final i8 [ $( ( $($prereqs:tt)+ ) )* ] $prereqs_unsigned:tt } => {
        /// - `i8`
        pub trait I8 :
            $( $($prereqs)+ + )*
        { }
        m! { @traits_Exactly_get I8 i8 get_i8 }
    };

    { @traits_Exactly_final i16 [ $( ( $($prereqs:tt)+ ) )* ] $prereqs_unsigned:tt } => {
        /// - `i16`
        pub trait I16 :
            $( $($prereqs)+ + )*
        { }
        m! { @traits_Exactly_get I16 i16 get_i16 }
    };

    { @traits_Exactly_final i32 [ $( ( $($prereqs:tt)+ ) )* ] $prereqs_unsigned:tt } => {
        /// - `i32`
        pub trait I32 :
            $( $($prereqs)+ + )*
        { }
        m! { @traits_Exactly_get I32 i32 get_i32 }
    };

    { @traits_Exactly_final i64 [ $( ( $($prereqs:tt)+ ) )* ] $prereqs_unsigned:tt } => {
        /// - `i64`
        pub trait I64 :
            $( $($prereqs)+ + )*
        { }
        m! { @traits_Exactly_get I64 i64 get_i64 }
    };

    { @traits_Exactly_final i128 [ $( ( $($prereqs:tt)+ ) )* ] $prereqs_unsigned:tt } => {
        /// - `i128`
        pub trait I128 :
            $( $($prereqs)+ + )*
        { }
        m! { @traits_Exactly_get I128 i128 get_i128 }
    };

    //----------------------------- #[inline(always)] pub const fn get_uNN<T: UNN>(t: T) -> uNN { ... }

    { @traits_Exactly_get $exactlyTrait:ident $type_name:ident $get_fn_name:ident } => {
        #[inline(always)] pub const fn $get_fn_name <T: $exactlyTrait >(t: T) -> $type_name {
            unsafe { ::core::mem::transmute_copy(&t) }
        }
    };

    //============================================================== trait impls

    { @trait_impls $types_info:tt
        {
            KWPIU: [ $( ( $kwpiu_type_name:ident, $kwpiu_bits:tt, $kwpiu_bits_log2:expr $(, $($restu:tt)* )? ) ),* ],
            KWPIS: [ $( ( $kwpis_type_name:ident, $kwpis_bits:tt, $kwpis_bits_log2:expr $(, $($rests:tt)* )? ) ),* ],
            $( $k:ident : $v:tt ),*
        }
    } => {
        $( m! { @trait_impl KWPIU ( $kwpiu_type_name, $kwpiu_bits, $kwpiu_bits_log2, ) } )*
        $( m! { @trait_impl KWPIS ( $kwpis_type_name, $kwpis_bits, $kwpis_bits_log2, ) } )*
    };

    { @trait_impl $kwpiu_kwpis:ident $kwpi_type_details:tt } => {
        m! { @trait_impl_2 $kwpiu_kwpis $kwpi_type_details $kwpi_type_details }
    };

    //--------------------------------------------------------------------- both unsigned and signed

    { @trait_impl_KWPI
        ( $type_name:ident, $bits:tt, $bits_log2:tt $(, $($rest:tt)* )? )
    } => {
        impl Integer for $type_name {
            type Type = $type_name;

            const BITS_LOG2: u16 = $bits_log2;
            const BITS: u32 = $type_name ::BITS;
            const MAX: Self::Type = $type_name ::MAX;
            const MIN: Self::Type = $type_name ::MIN;
            const ZERO: Self::Type = 0;
        }
    };

    //------------------------------------------------------------------------------------- unsigned

    { @trait_impl_2 KWPIU
        $kwpi_type_details:tt
        ( $type_name:ident, $bits:tt, $bits_log2:tt $(, $($rest:tt)* )? )
    } => {
        m! { @trait_impl_KWPI $kwpi_type_details }
        impl UnsignedInteger for $type_name { }
        m! { @trait_impl_KWPIU2 $bits $type_name }
    };

    { @impl_trait_KWPIU_AtLeast   8 $type_name:ident } => {
        impl UGte8 for $type_name { }
    };

    { @impl_trait_KWPIU_AtLeast  16 $type_name:ident } => {
        m! { @impl_trait_KWPIU_AtLeast 8 $type_name }
        impl UGte16 for $type_name { }
    };

    { @impl_trait_KWPIU_AtLeast  32 $type_name:ident } => {
        m! { @impl_trait_KWPIU_AtLeast 16 $type_name }
        impl UGte32 for $type_name { }
    };

    { @impl_trait_KWPIU_AtLeast  64 $type_name:ident } => {
        m! { @impl_trait_KWPIU_AtLeast 32 $type_name }
        impl UGte64 for $type_name { }
    };

    { @impl_trait_KWPIU_AtLeast 128 $type_name:ident } => {
        m! { @impl_trait_KWPIU_AtLeast 64 $type_name }
        impl UGte128 for $type_name { }
    };

    { @impl_trait_UEq   8 $type_name:ident } => { impl U8 for $type_name { } };
    { @impl_trait_UEq  16 $type_name:ident } => { impl U16 for $type_name { } };
    { @impl_trait_UEq  32 $type_name:ident } => { impl U32 for $type_name { } };
    { @impl_trait_UEq  64 $type_name:ident } => { impl U64 for $type_name { } };
    { @impl_trait_UEq 128 $type_name:ident } => { impl U128 for $type_name { } };

    { @trait_impl_KWPIU2 $bits:tt $type_name:ident } => {
        m! { @impl_trait_KWPIU_AtLeast $bits $type_name }
        m! { @impl_trait_UEq $bits $type_name }
        m! { @impl_trait_KWPIU_AtMost $bits $type_name }
    };

    { @impl_trait_KWPIU_AtMost 128 $type_name:ident } => {
        impl ULte128 for $type_name { }
    };

    { @impl_trait_KWPIU_AtMost  64 $type_name:ident } => {
        impl ULte64 for $type_name { }
        m! { @impl_trait_KWPIU_AtMost 128 $type_name }
    };

    { @impl_trait_KWPIU_AtMost  32 $type_name:ident } => {
        impl ULte32 for $type_name { }
        m! { @impl_trait_KWPIU_AtMost 64 $type_name }
    };

    { @impl_trait_KWPIU_AtMost  16 $type_name:ident } => {
        impl ULte16 for $type_name { }
        m! { @impl_trait_KWPIU_AtMost 32 $type_name }
    };

    { @impl_trait_KWPIU_AtMost   8 $type_name:ident } => {
        impl ULte8 for $type_name { }
        m! { @impl_trait_KWPIU_AtMost 16 $type_name }
    };

    //--------------------------------------------------------------------------------------- signed

    { @trait_impl_2 KWPIS
        $kwpi_type_details:tt
        ( $type_name:ident, $bits:tt, $bits_log2:tt $(, $($rest:tt)* )? )
    } => {
        m! { @trait_impl_KWPI $kwpi_type_details }
        impl SignedInteger for $type_name { }
        m! { @trait_impl_KWPIS2 $bits $type_name }
    };

    { @impl_trait_KWPIS_AtLeast   8 $type_name:ident } => {
        impl IGte8 for $type_name { }
    };

    { @impl_trait_KWPIS_AtLeast  16 $type_name:ident } => {
        m! { @impl_trait_KWPIS_AtLeast 8 $type_name }
        impl IGte16 for $type_name { }
    };

    { @impl_trait_KWPIS_AtLeast  32 $type_name:ident } => {
        m! { @impl_trait_KWPIS_AtLeast 16 $type_name }
        impl IGte32 for $type_name { }
    };

    { @impl_trait_KWPIS_AtLeast  64 $type_name:ident } => {
        m! { @impl_trait_KWPIS_AtLeast 32 $type_name }
        impl IGte64 for $type_name { }
    };

    { @impl_trait_KWPIS_AtLeast 128 $type_name:ident } => {
        m! { @impl_trait_KWPIS_AtLeast 64 $type_name }
        impl IGte128 for $type_name { }
    };

    { @impl_trait_IEq   8 $type_name:ident } => { impl I8 for $type_name { } };
    { @impl_trait_IEq  16 $type_name:ident } => { impl I16 for $type_name { } };
    { @impl_trait_IEq  32 $type_name:ident } => { impl I32 for $type_name { } };
    { @impl_trait_IEq  64 $type_name:ident } => { impl I64 for $type_name { } };
    { @impl_trait_IEq 128 $type_name:ident } => { impl I128 for $type_name { } };

    { @trait_impl_KWPIS2 $bits:tt $type_name:ident } => {
        m! { @impl_trait_KWPIS_AtLeast $bits $type_name }
        m! { @impl_trait_IEq $bits $type_name }
        m! { @impl_trait_KWPIS_AtMost $bits $type_name }
    };

    { @impl_trait_KWPIS_AtMost 128 $type_name:ident } => {
        impl ILte128 for $type_name { }
    };

    { @impl_trait_KWPIS_AtMost  64 $type_name:ident } => {
        impl ILte64 for $type_name { }
        m! { @impl_trait_KWPIS_AtMost 128 $type_name }
    };

    { @impl_trait_KWPIS_AtMost  32 $type_name:ident } => {
        impl ILte32 for $type_name { }
        m! { @impl_trait_KWPIS_AtMost 64 $type_name }
    };

    { @impl_trait_KWPIS_AtMost  16 $type_name:ident } => {
        impl ILte16 for $type_name { }
        m! { @impl_trait_KWPIS_AtMost 32 $type_name }
    };

    { @impl_trait_KWPIS_AtMost   8 $type_name:ident } => {
        impl ILte8 for $type_name { }
        m! { @impl_trait_KWPIS_AtMost 16 $type_name }
    };
}

// invoke the top-level m! macro
m! { }

// Count ones, zeros

#[inline] pub const fn count_ones_u8  <T: U8  >(t: T) -> u32  { get_u8  (t).count_ones() }
#[inline] pub const fn count_ones_u16 <T: U16 >(t: T) -> u32  { get_u16 (t).count_ones() }
#[inline] pub const fn count_ones_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).count_ones() }
#[inline] pub const fn count_ones_u64 <T: U64 >(t: T) -> u32  { get_u64 (t).count_ones() }
#[inline] pub const fn count_ones_u128<T: U128>(t: T) -> u32  { get_u128(t).count_ones() }
#[inline] pub const fn count_ones_i8  <T: I8  >(t: T) -> u32  { get_i8  (t).count_ones() }
#[inline] pub const fn count_ones_i16 <T: I16 >(t: T) -> u32  { get_i16 (t).count_ones() }
#[inline] pub const fn count_ones_i32 <T: I32 >(t: T) -> u32  { get_i32 (t).count_ones() }
#[inline] pub const fn count_ones_i64 <T: I64 >(t: T) -> u32  { get_i64 (t).count_ones() }
#[inline] pub const fn count_ones_i128<T: I128>(t: T) -> u32  { get_i128(t).count_ones() }

#[inline] pub const fn count_zeros_u8  <T: U8  >(t: T) -> u32 { get_u8  (t).count_zeros() }
#[inline] pub const fn count_zeros_u16 <T: U16 >(t: T) -> u32 { get_u16 (t).count_zeros() }
#[inline] pub const fn count_zeros_u32 <T: U32 >(t: T) -> u32 { get_u32 (t).count_zeros() }
#[inline] pub const fn count_zeros_u64 <T: U64 >(t: T) -> u32 { get_u64 (t).count_zeros() }
#[inline] pub const fn count_zeros_u128<T: U128>(t: T) -> u32 { get_u128(t).count_zeros() }
#[inline] pub const fn count_zeros_i8  <T: I8  >(t: T) -> u32 { get_i8  (t).count_zeros() }
#[inline] pub const fn count_zeros_i16 <T: I16 >(t: T) -> u32 { get_i16 (t).count_zeros() }
#[inline] pub const fn count_zeros_i32 <T: I32 >(t: T) -> u32 { get_i32 (t).count_zeros() }
#[inline] pub const fn count_zeros_i64 <T: I64 >(t: T) -> u32 { get_i64 (t).count_zeros() }
#[inline] pub const fn count_zeros_i128<T: I128>(t: T) -> u32 { get_i128(t).count_zeros() }

// Leading ones, zeros

#[inline] pub const fn leading_ones_u8  <T: U8  >(t: T) -> u32  { get_u8  (t).leading_ones() }
#[inline] pub const fn leading_ones_u16 <T: U16 >(t: T) -> u32  { get_u16 (t).leading_ones() }
#[inline] pub const fn leading_ones_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).leading_ones() }
#[inline] pub const fn leading_ones_u64 <T: U64 >(t: T) -> u32  { get_u64 (t).leading_ones() }
#[inline] pub const fn leading_ones_u128<T: U128>(t: T) -> u32  { get_u128(t).leading_ones() }
#[inline] pub const fn leading_ones_i8  <T: I8  >(t: T) -> u32  { get_i8  (t).leading_ones() }
#[inline] pub const fn leading_ones_i16 <T: I16 >(t: T) -> u32  { get_i16 (t).leading_ones() }
#[inline] pub const fn leading_ones_i32 <T: I32 >(t: T) -> u32  { get_i32 (t).leading_ones() }
#[inline] pub const fn leading_ones_i64 <T: I64 >(t: T) -> u32  { get_i64 (t).leading_ones() }
#[inline] pub const fn leading_ones_i128<T: I128>(t: T) -> u32  { get_i128(t).leading_ones() }

#[inline] pub const fn leading_zeros_u8  <T: U8  >(t: T) -> u32 { get_u8  (t).leading_zeros() }
#[inline] pub const fn leading_zeros_u16 <T: U16 >(t: T) -> u32 { get_u16 (t).leading_zeros() }
#[inline] pub const fn leading_zeros_u32 <T: U32 >(t: T) -> u32 { get_u32 (t).leading_zeros() }
#[inline] pub const fn leading_zeros_u64 <T: U64 >(t: T) -> u32 { get_u64 (t).leading_zeros() }
#[inline] pub const fn leading_zeros_u128<T: U128>(t: T) -> u32 { get_u128(t).leading_zeros() }
#[inline] pub const fn leading_zeros_i8  <T: I8  >(t: T) -> u32 { get_i8  (t).leading_zeros() }
#[inline] pub const fn leading_zeros_i16 <T: I16 >(t: T) -> u32 { get_i16 (t).leading_zeros() }
#[inline] pub const fn leading_zeros_i32 <T: I32 >(t: T) -> u32 { get_i32 (t).leading_zeros() }
#[inline] pub const fn leading_zeros_i64 <T: I64 >(t: T) -> u32 { get_i64 (t).leading_zeros() }
#[inline] pub const fn leading_zeros_i128<T: I128>(t: T) -> u32 { get_i128(t).leading_zeros() }

// Trailing ones, zeros

#[inline] pub const fn trailing_ones_u8  <T: U8  >(t: T) -> u32  { get_u8  (t).trailing_ones() }
#[inline] pub const fn trailing_ones_u16 <T: U16 >(t: T) -> u32  { get_u16 (t).trailing_ones() }
#[inline] pub const fn trailing_ones_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).trailing_ones() }
#[inline] pub const fn trailing_ones_u64 <T: U64 >(t: T) -> u32  { get_u64 (t).trailing_ones() }
#[inline] pub const fn trailing_ones_u128<T: U128>(t: T) -> u32  { get_u128(t).trailing_ones() }
#[inline] pub const fn trailing_ones_i8  <T: I8  >(t: T) -> u32  { get_i8  (t).trailing_ones() }
#[inline] pub const fn trailing_ones_i16 <T: I16 >(t: T) -> u32  { get_i16 (t).trailing_ones() }
#[inline] pub const fn trailing_ones_i32 <T: I32 >(t: T) -> u32  { get_i32 (t).trailing_ones() }
#[inline] pub const fn trailing_ones_i64 <T: I64 >(t: T) -> u32  { get_i64 (t).trailing_ones() }
#[inline] pub const fn trailing_ones_i128<T: I128>(t: T) -> u32  { get_i128(t).trailing_ones() }

#[inline] pub const fn trailing_zeros_u8  <T: U8  >(t: T) -> u32 { get_u8  (t).trailing_zeros() }
#[inline] pub const fn trailing_zeros_u16 <T: U16 >(t: T) -> u32 { get_u16 (t).trailing_zeros() }
#[inline] pub const fn trailing_zeros_u32 <T: U32 >(t: T) -> u32 { get_u32 (t).trailing_zeros() }
#[inline] pub const fn trailing_zeros_u64 <T: U64 >(t: T) -> u32 { get_u64 (t).trailing_zeros() }
#[inline] pub const fn trailing_zeros_u128<T: U128>(t: T) -> u32 { get_u128(t).trailing_zeros() }
#[inline] pub const fn trailing_zeros_i8  <T: I8  >(t: T) -> u32 { get_i8  (t).trailing_zeros() }
#[inline] pub const fn trailing_zeros_i16 <T: I16 >(t: T) -> u32 { get_i16 (t).trailing_zeros() }
#[inline] pub const fn trailing_zeros_i32 <T: I32 >(t: T) -> u32 { get_i32 (t).trailing_zeros() }
#[inline] pub const fn trailing_zeros_i64 <T: I64 >(t: T) -> u32 { get_i64 (t).trailing_zeros() }
#[inline] pub const fn trailing_zeros_i128<T: I128>(t: T) -> u32 { get_i128(t).trailing_zeros() }

// Cast

#[inline] pub const fn cast_signed_u8  <  T: U8  >(t: T) -> i8   { get_u8  (t).cast_signed() }
#[inline] pub const fn cast_signed_u16 <  T: U16 >(t: T) -> i16  { get_u16 (t).cast_signed() }
#[inline] pub const fn cast_signed_u32 <  T: U32 >(t: T) -> i32  { get_u32 (t).cast_signed() }
#[inline] pub const fn cast_signed_u64 <  T: U64 >(t: T) -> i64  { get_u64 (t).cast_signed() }
#[inline] pub const fn cast_signed_u128<  T: U128>(t: T) -> i128 { get_u128(t).cast_signed() }
#[inline] pub const fn cast_unsigned_i8  <T: I8  >(t: T) -> u8   { get_i8  (t).cast_unsigned() }
#[inline] pub const fn cast_unsigned_i16 <T: I16 >(t: T) -> u16  { get_i16 (t).cast_unsigned() }
#[inline] pub const fn cast_unsigned_i32 <T: I32 >(t: T) -> u32  { get_i32 (t).cast_unsigned() }
#[inline] pub const fn cast_unsigned_i64 <T: I64 >(t: T) -> u64  { get_i64 (t).cast_unsigned() }
#[inline] pub const fn cast_unsigned_i128<T: I128>(t: T) -> u128 { get_i128(t).cast_unsigned() }

// Reverse/swap bits/bytes

#[inline] pub const fn reverse_bits_u8  <T: U8  >(t: T) -> u8   { get_u8  (t).reverse_bits() }
#[inline] pub const fn reverse_bits_u16 <T: U16 >(t: T) -> u16  { get_u16 (t).reverse_bits() }
#[inline] pub const fn reverse_bits_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).reverse_bits() }
#[inline] pub const fn reverse_bits_u64 <T: U64 >(t: T) -> u64  { get_u64 (t).reverse_bits() }
#[inline] pub const fn reverse_bits_u128<T: U128>(t: T) -> u128 { get_u128(t).reverse_bits() }
#[inline] pub const fn reverse_bits_i8  <T: I8  >(t: T) -> i8   { get_i8  (t).reverse_bits() }
#[inline] pub const fn reverse_bits_i16 <T: I16 >(t: T) -> i16  { get_i16 (t).reverse_bits() }
#[inline] pub const fn reverse_bits_i32 <T: I32 >(t: T) -> i32  { get_i32 (t).reverse_bits() }
#[inline] pub const fn reverse_bits_i64 <T: I64 >(t: T) -> i64  { get_i64 (t).reverse_bits() }
#[inline] pub const fn reverse_bits_i128<T: I128>(t: T) -> i128 { get_i128(t).reverse_bits() }

#[inline] pub const fn swap_bytes_u8  <T: U8  >(t: T) -> u8   { get_u8  (t).swap_bytes() }
#[inline] pub const fn swap_bytes_u16 <T: U16 >(t: T) -> u16  { get_u16 (t).swap_bytes() }
#[inline] pub const fn swap_bytes_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).swap_bytes() }
#[inline] pub const fn swap_bytes_u64 <T: U64 >(t: T) -> u64  { get_u64 (t).swap_bytes() }
#[inline] pub const fn swap_bytes_u128<T: U128>(t: T) -> u128 { get_u128(t).swap_bytes() }
#[inline] pub const fn swap_bytes_i8  <T: I8  >(t: T) -> i8   { get_i8  (t).swap_bytes() }
#[inline] pub const fn swap_bytes_i16 <T: I16 >(t: T) -> i16  { get_i16 (t).swap_bytes() }
#[inline] pub const fn swap_bytes_i32 <T: I32 >(t: T) -> i32  { get_i32 (t).swap_bytes() }
#[inline] pub const fn swap_bytes_i64 <T: I64 >(t: T) -> i64  { get_i64 (t).swap_bytes() }
#[inline] pub const fn swap_bytes_i128<T: I128>(t: T) -> i128 { get_i128(t).swap_bytes() }

// Checked add/sub/neg/mul/div/rem/ilog/pow/shl/shr/next_multiple_of/next_power_of_two

#[inline] pub const fn checked_add_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> Option<u8  > { get_u8  (lhs).checked_add(get_u8  (rhs)) }
#[inline] pub const fn checked_add_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> Option<u16 > { get_u16 (lhs).checked_add(get_u16 (rhs)) }
#[inline] pub const fn checked_add_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> Option<u32 > { get_u32 (lhs).checked_add(get_u32 (rhs)) }
#[inline] pub const fn checked_add_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> Option<u64 > { get_u64 (lhs).checked_add(get_u64 (rhs)) }
#[inline] pub const fn checked_add_u128<T: U128, U: U128>(lhs: T, rhs: U) -> Option<u128> { get_u128(lhs).checked_add(get_u128(rhs)) }
#[inline] pub const fn checked_add_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> Option<i8  > { get_i8  (lhs).checked_add(get_i8  (rhs)) }
#[inline] pub const fn checked_add_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> Option<i16 > { get_i16 (lhs).checked_add(get_i16 (rhs)) }
#[inline] pub const fn checked_add_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> Option<i32 > { get_i32 (lhs).checked_add(get_i32 (rhs)) }
#[inline] pub const fn checked_add_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> Option<i64 > { get_i64 (lhs).checked_add(get_i64 (rhs)) }
#[inline] pub const fn checked_add_i128<T: I128, U: I128>(lhs: T, rhs: U) -> Option<i128> { get_i128(lhs).checked_add(get_i128(rhs)) }

#[inline] pub const fn checked_add_signed_u8    <T: U8,   U: I8  >(lhs: T, rhs: U) -> Option<u8  > { get_u8  (lhs).checked_add_signed(get_i8  (rhs)) }
#[inline] pub const fn checked_add_signed_u16   <T: U16,  U: I16 >(lhs: T, rhs: U) -> Option<u16 > { get_u16 (lhs).checked_add_signed(get_i16 (rhs)) }
#[inline] pub const fn checked_add_signed_u32   <T: U32,  U: I32 >(lhs: T, rhs: U) -> Option<u32 > { get_u32 (lhs).checked_add_signed(get_i32 (rhs)) }
#[inline] pub const fn checked_add_signed_u64   <T: U64,  U: I64 >(lhs: T, rhs: U) -> Option<u64 > { get_u64 (lhs).checked_add_signed(get_i64 (rhs)) }
#[inline] pub const fn checked_add_signed_u128  <T: U128, U: I128>(lhs: T, rhs: U) -> Option<u128> { get_u128(lhs).checked_add_signed(get_i128(rhs)) }
#[inline] pub const fn checked_add_unsigned_i8  <T: I8,   U: U8  >(lhs: T, rhs: U) -> Option<i8  > { get_i8  (lhs).checked_add_unsigned(get_u8  (rhs)) }
#[inline] pub const fn checked_add_unsigned_i16 <T: I16,  U: U16 >(lhs: T, rhs: U) -> Option<i16 > { get_i16 (lhs).checked_add_unsigned(get_u16 (rhs)) }
#[inline] pub const fn checked_add_unsigned_i32 <T: I32,  U: U32 >(lhs: T, rhs: U) -> Option<i32 > { get_i32 (lhs).checked_add_unsigned(get_u32 (rhs)) }
#[inline] pub const fn checked_add_unsigned_i64 <T: I64,  U: U64 >(lhs: T, rhs: U) -> Option<i64 > { get_i64 (lhs).checked_add_unsigned(get_u64 (rhs)) }
#[inline] pub const fn checked_add_unsigned_i128<T: I128, U: U128>(lhs: T, rhs: U) -> Option<i128> { get_i128(lhs).checked_add_unsigned(get_u128(rhs)) }

#[inline] pub const fn checked_sub_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> Option<u8  > { get_u8  (lhs).checked_sub(get_u8  (rhs)) }
#[inline] pub const fn checked_sub_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> Option<u16 > { get_u16 (lhs).checked_sub(get_u16 (rhs)) }
#[inline] pub const fn checked_sub_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> Option<u32 > { get_u32 (lhs).checked_sub(get_u32 (rhs)) }
#[inline] pub const fn checked_sub_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> Option<u64 > { get_u64 (lhs).checked_sub(get_u64 (rhs)) }
#[inline] pub const fn checked_sub_u128<T: U128, U: U128>(lhs: T, rhs: U) -> Option<u128> { get_u128(lhs).checked_sub(get_u128(rhs)) }
#[inline] pub const fn checked_sub_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> Option<i8  > { get_i8  (lhs).checked_sub(get_i8  (rhs)) }
#[inline] pub const fn checked_sub_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> Option<i16 > { get_i16 (lhs).checked_sub(get_i16 (rhs)) }
#[inline] pub const fn checked_sub_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> Option<i32 > { get_i32 (lhs).checked_sub(get_i32 (rhs)) }
#[inline] pub const fn checked_sub_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> Option<i64 > { get_i64 (lhs).checked_sub(get_i64 (rhs)) }
#[inline] pub const fn checked_sub_i128<T: I128, U: I128>(lhs: T, rhs: U) -> Option<i128> { get_i128(lhs).checked_sub(get_i128(rhs)) }

#[inline] pub const fn checked_sub_signed_u8    <T: U8,   U: I8  >(lhs: T, rhs: U) -> Option<u8  > { get_u8  (lhs).checked_sub_signed(get_i8  (rhs)) }
#[inline] pub const fn checked_sub_signed_u16   <T: U16,  U: I16 >(lhs: T, rhs: U) -> Option<u16 > { get_u16 (lhs).checked_sub_signed(get_i16 (rhs)) }
#[inline] pub const fn checked_sub_signed_u32   <T: U32,  U: I32 >(lhs: T, rhs: U) -> Option<u32 > { get_u32 (lhs).checked_sub_signed(get_i32 (rhs)) }
#[inline] pub const fn checked_sub_signed_u64   <T: U64,  U: I64 >(lhs: T, rhs: U) -> Option<u64 > { get_u64 (lhs).checked_sub_signed(get_i64 (rhs)) }
#[inline] pub const fn checked_sub_signed_u128  <T: U128, U: I128>(lhs: T, rhs: U) -> Option<u128> { get_u128(lhs).checked_sub_signed(get_i128(rhs)) }
#[inline] pub const fn checked_sub_unsigned_i8  <T: I8,   U: U8  >(lhs: T, rhs: U) -> Option<i8  > { get_i8  (lhs).checked_sub_unsigned(get_u8  (rhs)) }
#[inline] pub const fn checked_sub_unsigned_i16 <T: I16,  U: U16 >(lhs: T, rhs: U) -> Option<i16 > { get_i16 (lhs).checked_sub_unsigned(get_u16 (rhs)) }
#[inline] pub const fn checked_sub_unsigned_i32 <T: I32,  U: U32 >(lhs: T, rhs: U) -> Option<i32 > { get_i32 (lhs).checked_sub_unsigned(get_u32 (rhs)) }
#[inline] pub const fn checked_sub_unsigned_i64 <T: I64,  U: U64 >(lhs: T, rhs: U) -> Option<i64 > { get_i64 (lhs).checked_sub_unsigned(get_u64 (rhs)) }
#[inline] pub const fn checked_sub_unsigned_i128<T: I128, U: U128>(lhs: T, rhs: U) -> Option<i128> { get_i128(lhs).checked_sub_unsigned(get_u128(rhs)) }

#[inline] pub const fn checked_neg_u8  <T: U8  >(t: T) -> Option<u8  > { get_u8  (t).checked_neg() }
#[inline] pub const fn checked_neg_u16 <T: U16 >(t: T) -> Option<u16 > { get_u16 (t).checked_neg() }
#[inline] pub const fn checked_neg_u32 <T: U32 >(t: T) -> Option<u32 > { get_u32 (t).checked_neg() }
#[inline] pub const fn checked_neg_u64 <T: U64 >(t: T) -> Option<u64 > { get_u64 (t).checked_neg() }
#[inline] pub const fn checked_neg_u128<T: U128>(t: T) -> Option<u128> { get_u128(t).checked_neg() }
#[inline] pub const fn checked_neg_i8  <T: I8  >(t: T) -> Option<i8  > { get_i8  (t).checked_neg() }
#[inline] pub const fn checked_neg_i16 <T: I16 >(t: T) -> Option<i16 > { get_i16 (t).checked_neg() }
#[inline] pub const fn checked_neg_i32 <T: I32 >(t: T) -> Option<i32 > { get_i32 (t).checked_neg() }
#[inline] pub const fn checked_neg_i64 <T: I64 >(t: T) -> Option<i64 > { get_i64 (t).checked_neg() }
#[inline] pub const fn checked_neg_i128<T: I128>(t: T) -> Option<i128> { get_i128(t).checked_neg() }

#[inline] pub const fn checked_mul_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> Option<u8  > { get_u8  (lhs).checked_mul(get_u8  (rhs)) }
#[inline] pub const fn checked_mul_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> Option<u16 > { get_u16 (lhs).checked_mul(get_u16 (rhs)) }
#[inline] pub const fn checked_mul_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> Option<u32 > { get_u32 (lhs).checked_mul(get_u32 (rhs)) }
#[inline] pub const fn checked_mul_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> Option<u64 > { get_u64 (lhs).checked_mul(get_u64 (rhs)) }
#[inline] pub const fn checked_mul_u128<T: U128, U: U128>(lhs: T, rhs: U) -> Option<u128> { get_u128(lhs).checked_mul(get_u128(rhs)) }
#[inline] pub const fn checked_mul_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> Option<i8  > { get_i8  (lhs).checked_mul(get_i8  (rhs)) }
#[inline] pub const fn checked_mul_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> Option<i16 > { get_i16 (lhs).checked_mul(get_i16 (rhs)) }
#[inline] pub const fn checked_mul_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> Option<i32 > { get_i32 (lhs).checked_mul(get_i32 (rhs)) }
#[inline] pub const fn checked_mul_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> Option<i64 > { get_i64 (lhs).checked_mul(get_i64 (rhs)) }
#[inline] pub const fn checked_mul_i128<T: I128, U: I128>(lhs: T, rhs: U) -> Option<i128> { get_i128(lhs).checked_mul(get_i128(rhs)) }

#[inline] pub const fn checked_div_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> Option<u8  > { get_u8  (lhs).checked_div(get_u8  (rhs)) }
#[inline] pub const fn checked_div_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> Option<u16 > { get_u16 (lhs).checked_div(get_u16 (rhs)) }
#[inline] pub const fn checked_div_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> Option<u32 > { get_u32 (lhs).checked_div(get_u32 (rhs)) }
#[inline] pub const fn checked_div_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> Option<u64 > { get_u64 (lhs).checked_div(get_u64 (rhs)) }
#[inline] pub const fn checked_div_u128<T: U128, U: U128>(lhs: T, rhs: U) -> Option<u128> { get_u128(lhs).checked_div(get_u128(rhs)) }
#[inline] pub const fn checked_div_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> Option<i8  > { get_i8  (lhs).checked_div(get_i8  (rhs)) }
#[inline] pub const fn checked_div_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> Option<i16 > { get_i16 (lhs).checked_div(get_i16 (rhs)) }
#[inline] pub const fn checked_div_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> Option<i32 > { get_i32 (lhs).checked_div(get_i32 (rhs)) }
#[inline] pub const fn checked_div_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> Option<i64 > { get_i64 (lhs).checked_div(get_i64 (rhs)) }
#[inline] pub const fn checked_div_i128<T: I128, U: I128>(lhs: T, rhs: U) -> Option<i128> { get_i128(lhs).checked_div(get_i128(rhs)) }

#[inline] pub const fn checked_div_euclid_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> Option<u8  > { get_u8  (lhs).checked_div_euclid(get_u8  (rhs)) }
#[inline] pub const fn checked_div_euclid_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> Option<u16 > { get_u16 (lhs).checked_div_euclid(get_u16 (rhs)) }
#[inline] pub const fn checked_div_euclid_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> Option<u32 > { get_u32 (lhs).checked_div_euclid(get_u32 (rhs)) }
#[inline] pub const fn checked_div_euclid_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> Option<u64 > { get_u64 (lhs).checked_div_euclid(get_u64 (rhs)) }
#[inline] pub const fn checked_div_euclid_u128<T: U128, U: U128>(lhs: T, rhs: U) -> Option<u128> { get_u128(lhs).checked_div_euclid(get_u128(rhs)) }
#[inline] pub const fn checked_div_euclid_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> Option<i8  > { get_i8  (lhs).checked_div_euclid(get_i8  (rhs)) }
#[inline] pub const fn checked_div_euclid_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> Option<i16 > { get_i16 (lhs).checked_div_euclid(get_i16 (rhs)) }
#[inline] pub const fn checked_div_euclid_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> Option<i32 > { get_i32 (lhs).checked_div_euclid(get_i32 (rhs)) }
#[inline] pub const fn checked_div_euclid_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> Option<i64 > { get_i64 (lhs).checked_div_euclid(get_i64 (rhs)) }
#[inline] pub const fn checked_div_euclid_i128<T: I128, U: I128>(lhs: T, rhs: U) -> Option<i128> { get_i128(lhs).checked_div_euclid(get_i128(rhs)) }

#[inline] pub const fn checked_rem_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> Option<u8  > { get_u8  (lhs).checked_rem(get_u8  (rhs)) }
#[inline] pub const fn checked_rem_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> Option<u16 > { get_u16 (lhs).checked_rem(get_u16 (rhs)) }
#[inline] pub const fn checked_rem_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> Option<u32 > { get_u32 (lhs).checked_rem(get_u32 (rhs)) }
#[inline] pub const fn checked_rem_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> Option<u64 > { get_u64 (lhs).checked_rem(get_u64 (rhs)) }
#[inline] pub const fn checked_rem_u128<T: U128, U: U128>(lhs: T, rhs: U) -> Option<u128> { get_u128(lhs).checked_rem(get_u128(rhs)) }
#[inline] pub const fn checked_rem_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> Option<i8  > { get_i8  (lhs).checked_rem(get_i8  (rhs)) }
#[inline] pub const fn checked_rem_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> Option<i16 > { get_i16 (lhs).checked_rem(get_i16 (rhs)) }
#[inline] pub const fn checked_rem_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> Option<i32 > { get_i32 (lhs).checked_rem(get_i32 (rhs)) }
#[inline] pub const fn checked_rem_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> Option<i64 > { get_i64 (lhs).checked_rem(get_i64 (rhs)) }
#[inline] pub const fn checked_rem_i128<T: I128, U: I128>(lhs: T, rhs: U) -> Option<i128> { get_i128(lhs).checked_rem(get_i128(rhs)) }

#[inline] pub const fn checked_rem_euclid_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> Option<u8  > { get_u8  (lhs).checked_rem_euclid(get_u8  (rhs)) }
#[inline] pub const fn checked_rem_euclid_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> Option<u16 > { get_u16 (lhs).checked_rem_euclid(get_u16 (rhs)) }
#[inline] pub const fn checked_rem_euclid_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> Option<u32 > { get_u32 (lhs).checked_rem_euclid(get_u32 (rhs)) }
#[inline] pub const fn checked_rem_euclid_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> Option<u64 > { get_u64 (lhs).checked_rem_euclid(get_u64 (rhs)) }
#[inline] pub const fn checked_rem_euclid_u128<T: U128, U: U128>(lhs: T, rhs: U) -> Option<u128> { get_u128(lhs).checked_rem_euclid(get_u128(rhs)) }
#[inline] pub const fn checked_rem_euclid_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> Option<i8  > { get_i8  (lhs).checked_rem_euclid(get_i8  (rhs)) }
#[inline] pub const fn checked_rem_euclid_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> Option<i16 > { get_i16 (lhs).checked_rem_euclid(get_i16 (rhs)) }
#[inline] pub const fn checked_rem_euclid_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> Option<i32 > { get_i32 (lhs).checked_rem_euclid(get_i32 (rhs)) }
#[inline] pub const fn checked_rem_euclid_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> Option<i64 > { get_i64 (lhs).checked_rem_euclid(get_i64 (rhs)) }
#[inline] pub const fn checked_rem_euclid_i128<T: I128, U: I128>(lhs: T, rhs: U) -> Option<i128> { get_i128(lhs).checked_rem_euclid(get_i128(rhs)) }

#[inline] pub const fn checked_ilog_u8  <T: U8,   U: U8  >(t: T, base: U) -> Option<u32> { get_u8  (t).checked_ilog(get_u8  (base)) }
#[inline] pub const fn checked_ilog_u16 <T: U16,  U: U16 >(t: T, base: U) -> Option<u32> { get_u16 (t).checked_ilog(get_u16 (base)) }
#[inline] pub const fn checked_ilog_u32 <T: U32,  U: U32 >(t: T, base: U) -> Option<u32> { get_u32 (t).checked_ilog(get_u32 (base)) }
#[inline] pub const fn checked_ilog_u64 <T: U64,  U: U64 >(t: T, base: U) -> Option<u32> { get_u64 (t).checked_ilog(get_u64 (base)) }
#[inline] pub const fn checked_ilog_u128<T: U128, U: U128>(t: T, base: U) -> Option<u32> { get_u128(t).checked_ilog(get_u128(base)) }
#[inline] pub const fn checked_ilog_i8  <T: I8,   U: I8  >(t: T, base: U) -> Option<u32> { get_i8  (t).checked_ilog(get_i8  (base)) }
#[inline] pub const fn checked_ilog_i16 <T: I16,  U: I16 >(t: T, base: U) -> Option<u32> { get_i16 (t).checked_ilog(get_i16 (base)) }
#[inline] pub const fn checked_ilog_i32 <T: I32,  U: I32 >(t: T, base: U) -> Option<u32> { get_i32 (t).checked_ilog(get_i32 (base)) }
#[inline] pub const fn checked_ilog_i64 <T: I64,  U: I64 >(t: T, base: U) -> Option<u32> { get_i64 (t).checked_ilog(get_i64 (base)) }
#[inline] pub const fn checked_ilog_i128<T: I128, U: I128>(t: T, base: U) -> Option<u32> { get_i128(t).checked_ilog(get_i128(base)) }

#[inline] pub const fn checked_ilog2_u8  <T: U8  >(t: T) -> Option<u32> { get_u8  (t).checked_ilog2() }
#[inline] pub const fn checked_ilog2_u16 <T: U16 >(t: T) -> Option<u32> { get_u16 (t).checked_ilog2() }
#[inline] pub const fn checked_ilog2_u32 <T: U32 >(t: T) -> Option<u32> { get_u32 (t).checked_ilog2() }
#[inline] pub const fn checked_ilog2_u64 <T: U64 >(t: T) -> Option<u32> { get_u64 (t).checked_ilog2() }
#[inline] pub const fn checked_ilog2_u128<T: U128>(t: T) -> Option<u32> { get_u128(t).checked_ilog2() }
#[inline] pub const fn checked_ilog2_i8  <T: I8  >(t: T) -> Option<u32> { get_i8  (t).checked_ilog2() }
#[inline] pub const fn checked_ilog2_i16 <T: I16 >(t: T) -> Option<u32> { get_i16 (t).checked_ilog2() }
#[inline] pub const fn checked_ilog2_i32 <T: I32 >(t: T) -> Option<u32> { get_i32 (t).checked_ilog2() }
#[inline] pub const fn checked_ilog2_i64 <T: I64 >(t: T) -> Option<u32> { get_i64 (t).checked_ilog2() }
#[inline] pub const fn checked_ilog2_i128<T: I128>(t: T) -> Option<u32> { get_i128(t).checked_ilog2() }

#[inline] pub const fn checked_ilog10_u8  <T: U8  >(t: T) -> Option<u32> { get_u8  (t).checked_ilog10() }
#[inline] pub const fn checked_ilog10_u16 <T: U16 >(t: T) -> Option<u32> { get_u16 (t).checked_ilog10() }
#[inline] pub const fn checked_ilog10_u32 <T: U32 >(t: T) -> Option<u32> { get_u32 (t).checked_ilog10() }
#[inline] pub const fn checked_ilog10_u64 <T: U64 >(t: T) -> Option<u32> { get_u64 (t).checked_ilog10() }
#[inline] pub const fn checked_ilog10_u128<T: U128>(t: T) -> Option<u32> { get_u128(t).checked_ilog10() }
#[inline] pub const fn checked_ilog10_i8  <T: I8  >(t: T) -> Option<u32> { get_i8  (t).checked_ilog10() }
#[inline] pub const fn checked_ilog10_i16 <T: I16 >(t: T) -> Option<u32> { get_i16 (t).checked_ilog10() }
#[inline] pub const fn checked_ilog10_i32 <T: I32 >(t: T) -> Option<u32> { get_i32 (t).checked_ilog10() }
#[inline] pub const fn checked_ilog10_i64 <T: I64 >(t: T) -> Option<u32> { get_i64 (t).checked_ilog10() }
#[inline] pub const fn checked_ilog10_i128<T: I128>(t: T) -> Option<u32> { get_i128(t).checked_ilog10() }

#[inline] pub const fn checked_pow_u8  <T: U8  >(t: T, exp: u32) -> Option<u8  > { get_u8  (t).checked_pow(exp) }
#[inline] pub const fn checked_pow_u16 <T: U16 >(t: T, exp: u32) -> Option<u16 > { get_u16 (t).checked_pow(exp) }
#[inline] pub const fn checked_pow_u32 <T: U32 >(t: T, exp: u32) -> Option<u32 > { get_u32 (t).checked_pow(exp) }
#[inline] pub const fn checked_pow_u64 <T: U64 >(t: T, exp: u32) -> Option<u64 > { get_u64 (t).checked_pow(exp) }
#[inline] pub const fn checked_pow_u128<T: U128>(t: T, exp: u32) -> Option<u128> { get_u128(t).checked_pow(exp) }
#[inline] pub const fn checked_pow_i8  <T: I8  >(t: T, exp: u32) -> Option<i8  > { get_i8  (t).checked_pow(exp) }
#[inline] pub const fn checked_pow_i16 <T: I16 >(t: T, exp: u32) -> Option<i16 > { get_i16 (t).checked_pow(exp) }
#[inline] pub const fn checked_pow_i32 <T: I32 >(t: T, exp: u32) -> Option<i32 > { get_i32 (t).checked_pow(exp) }
#[inline] pub const fn checked_pow_i64 <T: I64 >(t: T, exp: u32) -> Option<i64 > { get_i64 (t).checked_pow(exp) }
#[inline] pub const fn checked_pow_i128<T: I128>(t: T, exp: u32) -> Option<i128> { get_i128(t).checked_pow(exp) }

#[inline] pub const fn checked_shl_u8  <T: U8,   N: U32>(t: T, n: N) -> Option<u8  > { get_u8  (t).checked_shl(get_u32(n)) }
#[inline] pub const fn checked_shl_u16 <T: U16,  N: U32>(t: T, n: N) -> Option<u16 > { get_u16 (t).checked_shl(get_u32(n)) }
#[inline] pub const fn checked_shl_u32 <T: U32,  N: U32>(t: T, n: N) -> Option<u32 > { get_u32 (t).checked_shl(get_u32(n)) }
#[inline] pub const fn checked_shl_u64 <T: U64,  N: U32>(t: T, n: N) -> Option<u64 > { get_u64 (t).checked_shl(get_u32(n)) }
#[inline] pub const fn checked_shl_u128<T: U128, N: U32>(t: T, n: N) -> Option<u128> { get_u128(t).checked_shl(get_u32(n)) }
#[inline] pub const fn checked_shl_i8  <T: I8,   N: U32>(t: T, n: N) -> Option<i8  > { get_i8  (t).checked_shl(get_u32(n)) }
#[inline] pub const fn checked_shl_i16 <T: I16,  N: U32>(t: T, n: N) -> Option<i16 > { get_i16 (t).checked_shl(get_u32(n)) }
#[inline] pub const fn checked_shl_i32 <T: I32,  N: U32>(t: T, n: N) -> Option<i32 > { get_i32 (t).checked_shl(get_u32(n)) }
#[inline] pub const fn checked_shl_i64 <T: I64,  N: U32>(t: T, n: N) -> Option<i64 > { get_i64 (t).checked_shl(get_u32(n)) }
#[inline] pub const fn checked_shl_i128<T: I128, N: U32>(t: T, n: N) -> Option<i128> { get_i128(t).checked_shl(get_u32(n)) }

#[inline] pub const fn checked_shr_u8  <T: U8,   N: U32>(t: T, n: N) -> Option<u8  > { get_u8  (t).checked_shr(get_u32(n)) }
#[inline] pub const fn checked_shr_u16 <T: U16,  N: U32>(t: T, n: N) -> Option<u16 > { get_u16 (t).checked_shr(get_u32(n)) }
#[inline] pub const fn checked_shr_u32 <T: U32,  N: U32>(t: T, n: N) -> Option<u32 > { get_u32 (t).checked_shr(get_u32(n)) }
#[inline] pub const fn checked_shr_u64 <T: U64,  N: U32>(t: T, n: N) -> Option<u64 > { get_u64 (t).checked_shr(get_u32(n)) }
#[inline] pub const fn checked_shr_u128<T: U128, N: U32>(t: T, n: N) -> Option<u128> { get_u128(t).checked_shr(get_u32(n)) }
#[inline] pub const fn checked_shr_i8  <T: I8,   N: U32>(t: T, n: N) -> Option<i8  > { get_i8  (t).checked_shr(get_u32(n)) }
#[inline] pub const fn checked_shr_i16 <T: I16,  N: U32>(t: T, n: N) -> Option<i16 > { get_i16 (t).checked_shr(get_u32(n)) }
#[inline] pub const fn checked_shr_i32 <T: I32,  N: U32>(t: T, n: N) -> Option<i32 > { get_i32 (t).checked_shr(get_u32(n)) }
#[inline] pub const fn checked_shr_i64 <T: I64,  N: U32>(t: T, n: N) -> Option<i64 > { get_i64 (t).checked_shr(get_u32(n)) }
#[inline] pub const fn checked_shr_i128<T: I128, N: U32>(t: T, n: N) -> Option<i128> { get_i128(t).checked_shr(get_u32(n)) }

#[inline] pub const fn checked_next_multiple_of_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> Option<u8  > { get_u8  (lhs).checked_next_multiple_of(get_u8  (rhs)) }
#[inline] pub const fn checked_next_multiple_of_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> Option<u16 > { get_u16 (lhs).checked_next_multiple_of(get_u16 (rhs)) }
#[inline] pub const fn checked_next_multiple_of_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> Option<u32 > { get_u32 (lhs).checked_next_multiple_of(get_u32 (rhs)) }
#[inline] pub const fn checked_next_multiple_of_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> Option<u64 > { get_u64 (lhs).checked_next_multiple_of(get_u64 (rhs)) }
#[inline] pub const fn checked_next_multiple_of_u128<T: U128, U: U128>(lhs: T, rhs: U) -> Option<u128> { get_u128(lhs).checked_next_multiple_of(get_u128(rhs)) }

#[inline] pub const fn checked_next_power_of_two_u8  <T: U8  >(t: T) -> Option<u8  > { get_u8  (t).checked_next_power_of_two() }
#[inline] pub const fn checked_next_power_of_two_u16 <T: U16 >(t: T) -> Option<u16 > { get_u16 (t).checked_next_power_of_two() }
#[inline] pub const fn checked_next_power_of_two_u32 <T: U32 >(t: T) -> Option<u32 > { get_u32 (t).checked_next_power_of_two() }
#[inline] pub const fn checked_next_power_of_two_u64 <T: U64 >(t: T) -> Option<u64 > { get_u64 (t).checked_next_power_of_two() }
#[inline] pub const fn checked_next_power_of_two_u128<T: U128>(t: T) -> Option<u128> { get_u128(t).checked_next_power_of_two() }

// Unchecked add/sub/mul

#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_add_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).unchecked_add(get_u8  (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_add_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).unchecked_add(get_u16 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_add_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).unchecked_add(get_u32 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_add_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).unchecked_add(get_u64 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_add_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).unchecked_add(get_u128(rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_add_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).unchecked_add(get_i8  (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_add_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).unchecked_add(get_i16 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_add_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).unchecked_add(get_i32 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_add_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).unchecked_add(get_i64 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_add_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).unchecked_add(get_i128(rhs)) }

#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_sub_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).unchecked_sub(get_u8  (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_sub_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).unchecked_sub(get_u16 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_sub_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).unchecked_sub(get_u32 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_sub_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).unchecked_sub(get_u64 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_sub_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).unchecked_sub(get_u128(rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_sub_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).unchecked_sub(get_i8  (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_sub_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).unchecked_sub(get_i16 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_sub_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).unchecked_sub(get_i32 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_sub_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).unchecked_sub(get_i64 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_sub_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).unchecked_sub(get_i128(rhs)) }

#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_mul_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).unchecked_mul(get_u8  (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_mul_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).unchecked_mul(get_u16 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_mul_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).unchecked_mul(get_u32 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_mul_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).unchecked_mul(get_u64 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_mul_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).unchecked_mul(get_u128(rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_mul_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).unchecked_mul(get_i8  (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_mul_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).unchecked_mul(get_i16 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_mul_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).unchecked_mul(get_i32 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_mul_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).unchecked_mul(get_i64 (rhs)) }
#[cfg(feature="rustversion")]#[rustversion::since(1.79)] pub const unsafe fn unchecked_mul_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).unchecked_mul(get_i128(rhs)) }

// Overflowing add/sub/neg/mul/div/rem/pow/neg/shl/shr

#[inline] pub const fn overflowing_add_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> (u8,   bool) { get_u8  (lhs).overflowing_add(get_u8  (rhs)) }
#[inline] pub const fn overflowing_add_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> (u16,  bool) { get_u16 (lhs).overflowing_add(get_u16 (rhs)) }
#[inline] pub const fn overflowing_add_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> (u32,  bool) { get_u32 (lhs).overflowing_add(get_u32 (rhs)) }
#[inline] pub const fn overflowing_add_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> (u64,  bool) { get_u64 (lhs).overflowing_add(get_u64 (rhs)) }
#[inline] pub const fn overflowing_add_u128<T: U128, U: U128>(lhs: T, rhs: U) -> (u128, bool) { get_u128(lhs).overflowing_add(get_u128(rhs)) }
#[inline] pub const fn overflowing_add_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> (i8,   bool) { get_i8  (lhs).overflowing_add(get_i8  (rhs)) }
#[inline] pub const fn overflowing_add_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> (i16,  bool) { get_i16 (lhs).overflowing_add(get_i16 (rhs)) }
#[inline] pub const fn overflowing_add_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> (i32,  bool) { get_i32 (lhs).overflowing_add(get_i32 (rhs)) }
#[inline] pub const fn overflowing_add_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> (i64,  bool) { get_i64 (lhs).overflowing_add(get_i64 (rhs)) }
#[inline] pub const fn overflowing_add_i128<T: I128, U: I128>(lhs: T, rhs: U) -> (i128, bool) { get_i128(lhs).overflowing_add(get_i128(rhs)) }

#[inline] pub const fn overflowing_add_signed_u8    <T: U8,   U: I8  >(lhs: T, rhs: U) -> (u8,   bool) { get_u8  (lhs).overflowing_add_signed(get_i8  (rhs)) }
#[inline] pub const fn overflowing_add_signed_u16   <T: U16,  U: I16 >(lhs: T, rhs: U) -> (u16,  bool) { get_u16 (lhs).overflowing_add_signed(get_i16 (rhs)) }
#[inline] pub const fn overflowing_add_signed_u32   <T: U32,  U: I32 >(lhs: T, rhs: U) -> (u32,  bool) { get_u32 (lhs).overflowing_add_signed(get_i32 (rhs)) }
#[inline] pub const fn overflowing_add_signed_u64   <T: U64,  U: I64 >(lhs: T, rhs: U) -> (u64,  bool) { get_u64 (lhs).overflowing_add_signed(get_i64 (rhs)) }
#[inline] pub const fn overflowing_add_signed_u128  <T: U128, U: I128>(lhs: T, rhs: U) -> (u128, bool) { get_u128(lhs).overflowing_add_signed(get_i128(rhs)) }
#[inline] pub const fn overflowing_add_unsigned_i8  <T: I8,   U: U8  >(lhs: T, rhs: U) -> (i8,   bool) { get_i8  (lhs).overflowing_add_unsigned(get_u8  (rhs)) }
#[inline] pub const fn overflowing_add_unsigned_i16 <T: I16,  U: U16 >(lhs: T, rhs: U) -> (i16,  bool) { get_i16 (lhs).overflowing_add_unsigned(get_u16 (rhs)) }
#[inline] pub const fn overflowing_add_unsigned_i32 <T: I32,  U: U32 >(lhs: T, rhs: U) -> (i32,  bool) { get_i32 (lhs).overflowing_add_unsigned(get_u32 (rhs)) }
#[inline] pub const fn overflowing_add_unsigned_i64 <T: I64,  U: U64 >(lhs: T, rhs: U) -> (i64,  bool) { get_i64 (lhs).overflowing_add_unsigned(get_u64 (rhs)) }
#[inline] pub const fn overflowing_add_unsigned_i128<T: I128, U: U128>(lhs: T, rhs: U) -> (i128, bool) { get_i128(lhs).overflowing_add_unsigned(get_u128(rhs)) }

#[inline] pub const fn overflowing_sub_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> (u8,   bool) { get_u8  (lhs).overflowing_sub(get_u8  (rhs)) }
#[inline] pub const fn overflowing_sub_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> (u16,  bool) { get_u16 (lhs).overflowing_sub(get_u16 (rhs)) }
#[inline] pub const fn overflowing_sub_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> (u32,  bool) { get_u32 (lhs).overflowing_sub(get_u32 (rhs)) }
#[inline] pub const fn overflowing_sub_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> (u64,  bool) { get_u64 (lhs).overflowing_sub(get_u64 (rhs)) }
#[inline] pub const fn overflowing_sub_u128<T: U128, U: U128>(lhs: T, rhs: U) -> (u128, bool) { get_u128(lhs).overflowing_sub(get_u128(rhs)) }
#[inline] pub const fn overflowing_sub_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> (i8,   bool) { get_i8  (lhs).overflowing_sub(get_i8  (rhs)) }
#[inline] pub const fn overflowing_sub_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> (i16,  bool) { get_i16 (lhs).overflowing_sub(get_i16 (rhs)) }
#[inline] pub const fn overflowing_sub_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> (i32,  bool) { get_i32 (lhs).overflowing_sub(get_i32 (rhs)) }
#[inline] pub const fn overflowing_sub_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> (i64,  bool) { get_i64 (lhs).overflowing_sub(get_i64 (rhs)) }
#[inline] pub const fn overflowing_sub_i128<T: I128, U: I128>(lhs: T, rhs: U) -> (i128, bool) { get_i128(lhs).overflowing_sub(get_i128(rhs)) }

#[inline] pub const fn overflowing_sub_signed_u8    <T: U8,   U: I8  >(lhs: T, rhs: U) -> (u8,   bool) { get_u8  (lhs).overflowing_sub_signed(get_i8  (rhs)) }
#[inline] pub const fn overflowing_sub_signed_u16   <T: U16,  U: I16 >(lhs: T, rhs: U) -> (u16,  bool) { get_u16 (lhs).overflowing_sub_signed(get_i16 (rhs)) }
#[inline] pub const fn overflowing_sub_signed_u32   <T: U32,  U: I32 >(lhs: T, rhs: U) -> (u32,  bool) { get_u32 (lhs).overflowing_sub_signed(get_i32 (rhs)) }
#[inline] pub const fn overflowing_sub_signed_u64   <T: U64,  U: I64 >(lhs: T, rhs: U) -> (u64,  bool) { get_u64 (lhs).overflowing_sub_signed(get_i64 (rhs)) }
#[inline] pub const fn overflowing_sub_signed_u128  <T: U128, U: I128>(lhs: T, rhs: U) -> (u128, bool) { get_u128(lhs).overflowing_sub_signed(get_i128(rhs)) }
#[inline] pub const fn overflowing_sub_unsigned_i8  <T: I8,   U: U8  >(lhs: T, rhs: U) -> (i8,   bool) { get_i8  (lhs).overflowing_sub_unsigned(get_u8  (rhs)) }
#[inline] pub const fn overflowing_sub_unsigned_i16 <T: I16,  U: U16 >(lhs: T, rhs: U) -> (i16,  bool) { get_i16 (lhs).overflowing_sub_unsigned(get_u16 (rhs)) }
#[inline] pub const fn overflowing_sub_unsigned_i32 <T: I32,  U: U32 >(lhs: T, rhs: U) -> (i32,  bool) { get_i32 (lhs).overflowing_sub_unsigned(get_u32 (rhs)) }
#[inline] pub const fn overflowing_sub_unsigned_i64 <T: I64,  U: U64 >(lhs: T, rhs: U) -> (i64,  bool) { get_i64 (lhs).overflowing_sub_unsigned(get_u64 (rhs)) }
#[inline] pub const fn overflowing_sub_unsigned_i128<T: I128, U: U128>(lhs: T, rhs: U) -> (i128, bool) { get_i128(lhs).overflowing_sub_unsigned(get_u128(rhs)) }

#[inline] pub const fn overflowing_neg_u8  <T: U8  >(t: T) -> (u8,   bool) { get_u8  (t).overflowing_neg() }
#[inline] pub const fn overflowing_neg_u16 <T: U16 >(t: T) -> (u16,  bool) { get_u16 (t).overflowing_neg() }
#[inline] pub const fn overflowing_neg_u32 <T: U32 >(t: T) -> (u32,  bool) { get_u32 (t).overflowing_neg() }
#[inline] pub const fn overflowing_neg_u64 <T: U64 >(t: T) -> (u64,  bool) { get_u64 (t).overflowing_neg() }
#[inline] pub const fn overflowing_neg_u128<T: U128>(t: T) -> (u128, bool) { get_u128(t).overflowing_neg() }
#[inline] pub const fn overflowing_neg_i8  <T: I8  >(t: T) -> (i8,   bool) { get_i8  (t).overflowing_neg() }
#[inline] pub const fn overflowing_neg_i16 <T: I16 >(t: T) -> (i16,  bool) { get_i16 (t).overflowing_neg() }
#[inline] pub const fn overflowing_neg_i32 <T: I32 >(t: T) -> (i32,  bool) { get_i32 (t).overflowing_neg() }
#[inline] pub const fn overflowing_neg_i64 <T: I64 >(t: T) -> (i64,  bool) { get_i64 (t).overflowing_neg() }
#[inline] pub const fn overflowing_neg_i128<T: I128>(t: T) -> (i128, bool) { get_i128(t).overflowing_neg() }

#[inline] pub const fn overflowing_mul_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> (u8,   bool) { get_u8  (lhs).overflowing_mul(get_u8  (rhs)) }
#[inline] pub const fn overflowing_mul_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> (u16,  bool) { get_u16 (lhs).overflowing_mul(get_u16 (rhs)) }
#[inline] pub const fn overflowing_mul_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> (u32,  bool) { get_u32 (lhs).overflowing_mul(get_u32 (rhs)) }
#[inline] pub const fn overflowing_mul_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> (u64,  bool) { get_u64 (lhs).overflowing_mul(get_u64 (rhs)) }
#[inline] pub const fn overflowing_mul_u128<T: U128, U: U128>(lhs: T, rhs: U) -> (u128, bool) { get_u128(lhs).overflowing_mul(get_u128(rhs)) }
#[inline] pub const fn overflowing_mul_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> (i8,   bool) { get_i8  (lhs).overflowing_mul(get_i8  (rhs)) }
#[inline] pub const fn overflowing_mul_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> (i16,  bool) { get_i16 (lhs).overflowing_mul(get_i16 (rhs)) }
#[inline] pub const fn overflowing_mul_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> (i32,  bool) { get_i32 (lhs).overflowing_mul(get_i32 (rhs)) }
#[inline] pub const fn overflowing_mul_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> (i64,  bool) { get_i64 (lhs).overflowing_mul(get_i64 (rhs)) }
#[inline] pub const fn overflowing_mul_i128<T: I128, U: I128>(lhs: T, rhs: U) -> (i128, bool) { get_i128(lhs).overflowing_mul(get_i128(rhs)) }

#[inline] pub const fn overflowing_div_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> (u8,   bool) { get_u8  (lhs).overflowing_div(get_u8  (rhs)) }
#[inline] pub const fn overflowing_div_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> (u16,  bool) { get_u16 (lhs).overflowing_div(get_u16 (rhs)) }
#[inline] pub const fn overflowing_div_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> (u32,  bool) { get_u32 (lhs).overflowing_div(get_u32 (rhs)) }
#[inline] pub const fn overflowing_div_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> (u64,  bool) { get_u64 (lhs).overflowing_div(get_u64 (rhs)) }
#[inline] pub const fn overflowing_div_u128<T: U128, U: U128>(lhs: T, rhs: U) -> (u128, bool) { get_u128(lhs).overflowing_div(get_u128(rhs)) }
#[inline] pub const fn overflowing_div_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> (i8,   bool) { get_i8  (lhs).overflowing_div(get_i8  (rhs)) }
#[inline] pub const fn overflowing_div_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> (i16,  bool) { get_i16 (lhs).overflowing_div(get_i16 (rhs)) }
#[inline] pub const fn overflowing_div_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> (i32,  bool) { get_i32 (lhs).overflowing_div(get_i32 (rhs)) }
#[inline] pub const fn overflowing_div_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> (i64,  bool) { get_i64 (lhs).overflowing_div(get_i64 (rhs)) }
#[inline] pub const fn overflowing_div_i128<T: I128, U: I128>(lhs: T, rhs: U) -> (i128, bool) { get_i128(lhs).overflowing_div(get_i128(rhs)) }

#[inline] pub const fn overflowing_div_euclid_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> (u8,   bool) { get_u8  (lhs).overflowing_div_euclid(get_u8  (rhs)) }
#[inline] pub const fn overflowing_div_euclid_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> (u16,  bool) { get_u16 (lhs).overflowing_div_euclid(get_u16 (rhs)) }
#[inline] pub const fn overflowing_div_euclid_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> (u32,  bool) { get_u32 (lhs).overflowing_div_euclid(get_u32 (rhs)) }
#[inline] pub const fn overflowing_div_euclid_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> (u64,  bool) { get_u64 (lhs).overflowing_div_euclid(get_u64 (rhs)) }
#[inline] pub const fn overflowing_div_euclid_u128<T: U128, U: U128>(lhs: T, rhs: U) -> (u128, bool) { get_u128(lhs).overflowing_div_euclid(get_u128(rhs)) }
#[inline] pub const fn overflowing_div_euclid_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> (i8,   bool) { get_i8  (lhs).overflowing_div_euclid(get_i8  (rhs)) }
#[inline] pub const fn overflowing_div_euclid_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> (i16,  bool) { get_i16 (lhs).overflowing_div_euclid(get_i16 (rhs)) }
#[inline] pub const fn overflowing_div_euclid_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> (i32,  bool) { get_i32 (lhs).overflowing_div_euclid(get_i32 (rhs)) }
#[inline] pub const fn overflowing_div_euclid_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> (i64,  bool) { get_i64 (lhs).overflowing_div_euclid(get_i64 (rhs)) }
#[inline] pub const fn overflowing_div_euclid_i128<T: I128, U: I128>(lhs: T, rhs: U) -> (i128, bool) { get_i128(lhs).overflowing_div_euclid(get_i128(rhs)) }

#[inline] pub const fn overflowing_rem_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> (u8,   bool) { get_u8  (lhs).overflowing_rem(get_u8  (rhs)) }
#[inline] pub const fn overflowing_rem_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> (u16,  bool) { get_u16 (lhs).overflowing_rem(get_u16 (rhs)) }
#[inline] pub const fn overflowing_rem_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> (u32,  bool) { get_u32 (lhs).overflowing_rem(get_u32 (rhs)) }
#[inline] pub const fn overflowing_rem_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> (u64,  bool) { get_u64 (lhs).overflowing_rem(get_u64 (rhs)) }
#[inline] pub const fn overflowing_rem_u128<T: U128, U: U128>(lhs: T, rhs: U) -> (u128, bool) { get_u128(lhs).overflowing_rem(get_u128(rhs)) }
#[inline] pub const fn overflowing_rem_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> (i8,   bool) { get_i8  (lhs).overflowing_rem(get_i8  (rhs)) }
#[inline] pub const fn overflowing_rem_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> (i16,  bool) { get_i16 (lhs).overflowing_rem(get_i16 (rhs)) }
#[inline] pub const fn overflowing_rem_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> (i32,  bool) { get_i32 (lhs).overflowing_rem(get_i32 (rhs)) }
#[inline] pub const fn overflowing_rem_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> (i64,  bool) { get_i64 (lhs).overflowing_rem(get_i64 (rhs)) }
#[inline] pub const fn overflowing_rem_i128<T: I128, U: I128>(lhs: T, rhs: U) -> (i128, bool) { get_i128(lhs).overflowing_rem(get_i128(rhs)) }

#[inline] pub const fn overflowing_rem_euclid_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> (u8,   bool) { get_u8  (lhs).overflowing_rem_euclid(get_u8  (rhs)) }
#[inline] pub const fn overflowing_rem_euclid_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> (u16,  bool) { get_u16 (lhs).overflowing_rem_euclid(get_u16 (rhs)) }
#[inline] pub const fn overflowing_rem_euclid_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> (u32,  bool) { get_u32 (lhs).overflowing_rem_euclid(get_u32 (rhs)) }
#[inline] pub const fn overflowing_rem_euclid_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> (u64,  bool) { get_u64 (lhs).overflowing_rem_euclid(get_u64 (rhs)) }
#[inline] pub const fn overflowing_rem_euclid_u128<T: U128, U: U128>(lhs: T, rhs: U) -> (u128, bool) { get_u128(lhs).overflowing_rem_euclid(get_u128(rhs)) }
#[inline] pub const fn overflowing_rem_euclid_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> (i8,   bool) { get_i8  (lhs).overflowing_rem_euclid(get_i8  (rhs)) }
#[inline] pub const fn overflowing_rem_euclid_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> (i16,  bool) { get_i16 (lhs).overflowing_rem_euclid(get_i16 (rhs)) }
#[inline] pub const fn overflowing_rem_euclid_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> (i32,  bool) { get_i32 (lhs).overflowing_rem_euclid(get_i32 (rhs)) }
#[inline] pub const fn overflowing_rem_euclid_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> (i64,  bool) { get_i64 (lhs).overflowing_rem_euclid(get_i64 (rhs)) }
#[inline] pub const fn overflowing_rem_euclid_i128<T: I128, U: I128>(lhs: T, rhs: U) -> (i128, bool) { get_i128(lhs).overflowing_rem_euclid(get_i128(rhs)) }

#[inline] pub const fn overflowing_pow_u8  <T: U8  >(t: T, exp: u32) -> (u8,   bool) { get_u8  (t).overflowing_pow(exp) }
#[inline] pub const fn overflowing_pow_u16 <T: U16 >(t: T, exp: u32) -> (u16,  bool) { get_u16 (t).overflowing_pow(exp) }
#[inline] pub const fn overflowing_pow_u32 <T: U32 >(t: T, exp: u32) -> (u32,  bool) { get_u32 (t).overflowing_pow(exp) }
#[inline] pub const fn overflowing_pow_u64 <T: U64 >(t: T, exp: u32) -> (u64,  bool) { get_u64 (t).overflowing_pow(exp) }
#[inline] pub const fn overflowing_pow_u128<T: U128>(t: T, exp: u32) -> (u128, bool) { get_u128(t).overflowing_pow(exp) }
#[inline] pub const fn overflowing_pow_i8  <T: I8  >(t: T, exp: u32) -> (i8,   bool) { get_i8  (t).overflowing_pow(exp) }
#[inline] pub const fn overflowing_pow_i16 <T: I16 >(t: T, exp: u32) -> (i16,  bool) { get_i16 (t).overflowing_pow(exp) }
#[inline] pub const fn overflowing_pow_i32 <T: I32 >(t: T, exp: u32) -> (i32,  bool) { get_i32 (t).overflowing_pow(exp) }
#[inline] pub const fn overflowing_pow_i64 <T: I64 >(t: T, exp: u32) -> (i64,  bool) { get_i64 (t).overflowing_pow(exp) }
#[inline] pub const fn overflowing_pow_i128<T: I128>(t: T, exp: u32) -> (i128, bool) { get_i128(t).overflowing_pow(exp) }

#[inline] pub const fn overflowing_shl_u8  <T: U8,   N: U32>(t: T, n: N) -> (u8,   bool) { get_u8  (t).overflowing_shl(get_u32(n)) }
#[inline] pub const fn overflowing_shl_u16 <T: U16,  N: U32>(t: T, n: N) -> (u16,  bool) { get_u16 (t).overflowing_shl(get_u32(n)) }
#[inline] pub const fn overflowing_shl_u32 <T: U32,  N: U32>(t: T, n: N) -> (u32,  bool) { get_u32 (t).overflowing_shl(get_u32(n)) }
#[inline] pub const fn overflowing_shl_u64 <T: U64,  N: U32>(t: T, n: N) -> (u64,  bool) { get_u64 (t).overflowing_shl(get_u32(n)) }
#[inline] pub const fn overflowing_shl_u128<T: U128, N: U32>(t: T, n: N) -> (u128, bool) { get_u128(t).overflowing_shl(get_u32(n)) }
#[inline] pub const fn overflowing_shl_i8  <T: I8,   N: U32>(t: T, n: N) -> (i8,   bool) { get_i8  (t).overflowing_shl(get_u32(n)) }
#[inline] pub const fn overflowing_shl_i16 <T: I16,  N: U32>(t: T, n: N) -> (i16,  bool) { get_i16 (t).overflowing_shl(get_u32(n)) }
#[inline] pub const fn overflowing_shl_i32 <T: I32,  N: U32>(t: T, n: N) -> (i32,  bool) { get_i32 (t).overflowing_shl(get_u32(n)) }
#[inline] pub const fn overflowing_shl_i64 <T: I64,  N: U32>(t: T, n: N) -> (i64,  bool) { get_i64 (t).overflowing_shl(get_u32(n)) }
#[inline] pub const fn overflowing_shl_i128<T: I128, N: U32>(t: T, n: N) -> (i128, bool) { get_i128(t).overflowing_shl(get_u32(n)) }

#[inline] pub const fn overflowing_shr_u8  <T: U8,   N: U32>(t: T, n: N) -> (u8,   bool) { get_u8  (t).overflowing_shr(get_u32(n)) }
#[inline] pub const fn overflowing_shr_u16 <T: U16,  N: U32>(t: T, n: N) -> (u16,  bool) { get_u16 (t).overflowing_shr(get_u32(n)) }
#[inline] pub const fn overflowing_shr_u32 <T: U32,  N: U32>(t: T, n: N) -> (u32,  bool) { get_u32 (t).overflowing_shr(get_u32(n)) }
#[inline] pub const fn overflowing_shr_u64 <T: U64,  N: U32>(t: T, n: N) -> (u64,  bool) { get_u64 (t).overflowing_shr(get_u32(n)) }
#[inline] pub const fn overflowing_shr_u128<T: U128, N: U32>(t: T, n: N) -> (u128, bool) { get_u128(t).overflowing_shr(get_u32(n)) }
#[inline] pub const fn overflowing_shr_i8  <T: I8,   N: U32>(t: T, n: N) -> (i8,   bool) { get_i8  (t).overflowing_shr(get_u32(n)) }
#[inline] pub const fn overflowing_shr_i16 <T: I16,  N: U32>(t: T, n: N) -> (i16,  bool) { get_i16 (t).overflowing_shr(get_u32(n)) }
#[inline] pub const fn overflowing_shr_i32 <T: I32,  N: U32>(t: T, n: N) -> (i32,  bool) { get_i32 (t).overflowing_shr(get_u32(n)) }
#[inline] pub const fn overflowing_shr_i64 <T: I64,  N: U32>(t: T, n: N) -> (i64,  bool) { get_i64 (t).overflowing_shr(get_u32(n)) }
#[inline] pub const fn overflowing_shr_i128<T: I128, N: U32>(t: T, n: N) -> (i128, bool) { get_i128(t).overflowing_shr(get_u32(n)) }

// Saturating add/sub/mul/div/pow

#[inline] pub const fn saturating_add_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).saturating_add(get_u8  (rhs)) }
#[inline] pub const fn saturating_add_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).saturating_add(get_u16 (rhs)) }
#[inline] pub const fn saturating_add_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).saturating_add(get_u32 (rhs)) }
#[inline] pub const fn saturating_add_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).saturating_add(get_u64 (rhs)) }
#[inline] pub const fn saturating_add_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).saturating_add(get_u128(rhs)) }
#[inline] pub const fn saturating_add_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).saturating_add(get_i8  (rhs)) }
#[inline] pub const fn saturating_add_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).saturating_add(get_i16 (rhs)) }
#[inline] pub const fn saturating_add_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).saturating_add(get_i32 (rhs)) }
#[inline] pub const fn saturating_add_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).saturating_add(get_i64 (rhs)) }
#[inline] pub const fn saturating_add_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).saturating_add(get_i128(rhs)) }

#[inline] pub const fn saturating_add_signed_u8  <T: U8,   U: I8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).saturating_add_signed(get_i8  (rhs)) }
#[inline] pub const fn saturating_add_signed_u16 <T: U16,  U: I16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).saturating_add_signed(get_i16 (rhs)) }
#[inline] pub const fn saturating_add_signed_u32 <T: U32,  U: I32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).saturating_add_signed(get_i32 (rhs)) }
#[inline] pub const fn saturating_add_signed_u64 <T: U64,  U: I64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).saturating_add_signed(get_i64 (rhs)) }
#[inline] pub const fn saturating_add_signed_u128<T: U128, U: I128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).saturating_add_signed(get_i128(rhs)) }
#[inline] pub const fn saturating_add_unsigned_i8  <T: I8,   U: U8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).saturating_add_unsigned(get_u8  (rhs)) }
#[inline] pub const fn saturating_add_unsigned_i16 <T: I16,  U: U16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).saturating_add_unsigned(get_u16 (rhs)) }
#[inline] pub const fn saturating_add_unsigned_i32 <T: I32,  U: U32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).saturating_add_unsigned(get_u32 (rhs)) }
#[inline] pub const fn saturating_add_unsigned_i64 <T: I64,  U: U64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).saturating_add_unsigned(get_u64 (rhs)) }
#[inline] pub const fn saturating_add_unsigned_i128<T: I128, U: U128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).saturating_add_unsigned(get_u128(rhs)) }

#[inline] pub const fn saturating_sub_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).saturating_sub(get_u8  (rhs)) }
#[inline] pub const fn saturating_sub_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).saturating_sub(get_u16 (rhs)) }
#[inline] pub const fn saturating_sub_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).saturating_sub(get_u32 (rhs)) }
#[inline] pub const fn saturating_sub_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).saturating_sub(get_u64 (rhs)) }
#[inline] pub const fn saturating_sub_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).saturating_sub(get_u128(rhs)) }
#[inline] pub const fn saturating_sub_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).saturating_sub(get_i8  (rhs)) }
#[inline] pub const fn saturating_sub_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).saturating_sub(get_i16 (rhs)) }
#[inline] pub const fn saturating_sub_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).saturating_sub(get_i32 (rhs)) }
#[inline] pub const fn saturating_sub_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).saturating_sub(get_i64 (rhs)) }
#[inline] pub const fn saturating_sub_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).saturating_sub(get_i128(rhs)) }

#[inline] pub const fn saturating_sub_signed_u8  <T: U8,   U: I8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).saturating_sub_signed(get_i8  (rhs)) }
#[inline] pub const fn saturating_sub_signed_u16 <T: U16,  U: I16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).saturating_sub_signed(get_i16 (rhs)) }
#[inline] pub const fn saturating_sub_signed_u32 <T: U32,  U: I32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).saturating_sub_signed(get_i32 (rhs)) }
#[inline] pub const fn saturating_sub_signed_u64 <T: U64,  U: I64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).saturating_sub_signed(get_i64 (rhs)) }
#[inline] pub const fn saturating_sub_signed_u128<T: U128, U: I128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).saturating_sub_signed(get_i128(rhs)) }
#[inline] pub const fn saturating_sub_unsigned_i8  <T: I8,   U: U8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).saturating_sub_unsigned(get_u8  (rhs)) }
#[inline] pub const fn saturating_sub_unsigned_i16 <T: I16,  U: U16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).saturating_sub_unsigned(get_u16 (rhs)) }
#[inline] pub const fn saturating_sub_unsigned_i32 <T: I32,  U: U32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).saturating_sub_unsigned(get_u32 (rhs)) }
#[inline] pub const fn saturating_sub_unsigned_i64 <T: I64,  U: U64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).saturating_sub_unsigned(get_u64 (rhs)) }
#[inline] pub const fn saturating_sub_unsigned_i128<T: I128, U: U128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).saturating_sub_unsigned(get_u128(rhs)) }

#[inline] pub const fn saturating_mul_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).saturating_mul(get_u8  (rhs)) }
#[inline] pub const fn saturating_mul_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).saturating_mul(get_u16 (rhs)) }
#[inline] pub const fn saturating_mul_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).saturating_mul(get_u32 (rhs)) }
#[inline] pub const fn saturating_mul_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).saturating_mul(get_u64 (rhs)) }
#[inline] pub const fn saturating_mul_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).saturating_mul(get_u128(rhs)) }
#[inline] pub const fn saturating_mul_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).saturating_mul(get_i8  (rhs)) }
#[inline] pub const fn saturating_mul_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).saturating_mul(get_i16 (rhs)) }
#[inline] pub const fn saturating_mul_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).saturating_mul(get_i32 (rhs)) }
#[inline] pub const fn saturating_mul_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).saturating_mul(get_i64 (rhs)) }
#[inline] pub const fn saturating_mul_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).saturating_mul(get_i128(rhs)) }

#[inline] pub const fn saturating_div_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).saturating_div(get_u8  (rhs)) }
#[inline] pub const fn saturating_div_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).saturating_div(get_u16 (rhs)) }
#[inline] pub const fn saturating_div_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).saturating_div(get_u32 (rhs)) }
#[inline] pub const fn saturating_div_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).saturating_div(get_u64 (rhs)) }
#[inline] pub const fn saturating_div_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).saturating_div(get_u128(rhs)) }
#[inline] pub const fn saturating_div_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).saturating_div(get_i8  (rhs)) }
#[inline] pub const fn saturating_div_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).saturating_div(get_i16 (rhs)) }
#[inline] pub const fn saturating_div_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).saturating_div(get_i32 (rhs)) }
#[inline] pub const fn saturating_div_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).saturating_div(get_i64 (rhs)) }
#[inline] pub const fn saturating_div_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).saturating_div(get_i128(rhs)) }

#[inline] pub const fn saturating_pow_u8  <T: U8  >(lhs: T, rhs: u32) -> u8   { get_u8  (lhs).saturating_pow(rhs) }
#[inline] pub const fn saturating_pow_u16 <T: U16 >(lhs: T, rhs: u32) -> u16  { get_u16 (lhs).saturating_pow(rhs) }
#[inline] pub const fn saturating_pow_u32 <T: U32 >(lhs: T, rhs: u32) -> u32  { get_u32 (lhs).saturating_pow(rhs) }
#[inline] pub const fn saturating_pow_u64 <T: U64 >(lhs: T, rhs: u32) -> u64  { get_u64 (lhs).saturating_pow(rhs) }
#[inline] pub const fn saturating_pow_u128<T: U128>(lhs: T, rhs: u32) -> u128 { get_u128(lhs).saturating_pow(rhs) }
#[inline] pub const fn saturating_pow_i8  <T: I8  >(lhs: T, rhs: u32) -> i8   { get_i8  (lhs).saturating_pow(rhs) }
#[inline] pub const fn saturating_pow_i16 <T: I16 >(lhs: T, rhs: u32) -> i16  { get_i16 (lhs).saturating_pow(rhs) }
#[inline] pub const fn saturating_pow_i32 <T: I32 >(lhs: T, rhs: u32) -> i32  { get_i32 (lhs).saturating_pow(rhs) }
#[inline] pub const fn saturating_pow_i64 <T: I64 >(lhs: T, rhs: u32) -> i64  { get_i64 (lhs).saturating_pow(rhs) }
#[inline] pub const fn saturating_pow_i128<T: I128>(lhs: T, rhs: u32) -> i128 { get_i128(lhs).saturating_pow(rhs) }

// Unbounded shl/shr

#[inline] pub const fn unbounded_shl_u8  <T: U8,   N: U32>(t: T, n: N) -> u8   { get_u8  (t).unbounded_shl(get_u32(n)) }
#[inline] pub const fn unbounded_shl_u16 <T: U16,  N: U32>(t: T, n: N) -> u16  { get_u16 (t).unbounded_shl(get_u32(n)) }
#[inline] pub const fn unbounded_shl_u32 <T: U32,  N: U32>(t: T, n: N) -> u32  { get_u32 (t).unbounded_shl(get_u32(n)) }
#[inline] pub const fn unbounded_shl_u64 <T: U64,  N: U32>(t: T, n: N) -> u64  { get_u64 (t).unbounded_shl(get_u32(n)) }
#[inline] pub const fn unbounded_shl_u128<T: U128, N: U32>(t: T, n: N) -> u128 { get_u128(t).unbounded_shl(get_u32(n)) }
#[inline] pub const fn unbounded_shl_i8  <T: I8,   N: U32>(t: T, n: N) -> i8   { get_i8  (t).unbounded_shl(get_u32(n)) }
#[inline] pub const fn unbounded_shl_i16 <T: I16,  N: U32>(t: T, n: N) -> i16  { get_i16 (t).unbounded_shl(get_u32(n)) }
#[inline] pub const fn unbounded_shl_i32 <T: I32,  N: U32>(t: T, n: N) -> i32  { get_i32 (t).unbounded_shl(get_u32(n)) }
#[inline] pub const fn unbounded_shl_i64 <T: I64,  N: U32>(t: T, n: N) -> i64  { get_i64 (t).unbounded_shl(get_u32(n)) }
#[inline] pub const fn unbounded_shl_i128<T: I128, N: U32>(t: T, n: N) -> i128 { get_i128(t).unbounded_shl(get_u32(n)) }

#[inline] pub const fn unbounded_shr_u8  <T: U8,   N: U32>(t: T, n: N) -> u8   { get_u8  (t).unbounded_shr(get_u32(n)) }
#[inline] pub const fn unbounded_shr_u16 <T: U16,  N: U32>(t: T, n: N) -> u16  { get_u16 (t).unbounded_shr(get_u32(n)) }
#[inline] pub const fn unbounded_shr_u32 <T: U32,  N: U32>(t: T, n: N) -> u32  { get_u32 (t).unbounded_shr(get_u32(n)) }
#[inline] pub const fn unbounded_shr_u64 <T: U64,  N: U32>(t: T, n: N) -> u64  { get_u64 (t).unbounded_shr(get_u32(n)) }
#[inline] pub const fn unbounded_shr_u128<T: U128, N: U32>(t: T, n: N) -> u128 { get_u128(t).unbounded_shr(get_u32(n)) }
#[inline] pub const fn unbounded_shr_i8  <T: I8,   N: U32>(t: T, n: N) -> i8   { get_i8  (t).unbounded_shr(get_u32(n)) }
#[inline] pub const fn unbounded_shr_i16 <T: I16,  N: U32>(t: T, n: N) -> i16  { get_i16 (t).unbounded_shr(get_u32(n)) }
#[inline] pub const fn unbounded_shr_i32 <T: I32,  N: U32>(t: T, n: N) -> i32  { get_i32 (t).unbounded_shr(get_u32(n)) }
#[inline] pub const fn unbounded_shr_i64 <T: I64,  N: U32>(t: T, n: N) -> i64  { get_i64 (t).unbounded_shr(get_u32(n)) }
#[inline] pub const fn unbounded_shr_i128<T: I128, N: U32>(t: T, n: N) -> i128 { get_i128(t).unbounded_shr(get_u32(n)) }

// Wrapping add/sub/neg/mul/div/rem/pow/shl/shr

#[inline] pub const fn wrapping_add_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).wrapping_add(get_u8  (rhs)) }
#[inline] pub const fn wrapping_add_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).wrapping_add(get_u16 (rhs)) }
#[inline] pub const fn wrapping_add_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).wrapping_add(get_u32 (rhs)) }
#[inline] pub const fn wrapping_add_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).wrapping_add(get_u64 (rhs)) }
#[inline] pub const fn wrapping_add_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).wrapping_add(get_u128(rhs)) }
#[inline] pub const fn wrapping_add_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).wrapping_add(get_i8  (rhs)) }
#[inline] pub const fn wrapping_add_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).wrapping_add(get_i16 (rhs)) }
#[inline] pub const fn wrapping_add_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).wrapping_add(get_i32 (rhs)) }
#[inline] pub const fn wrapping_add_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).wrapping_add(get_i64 (rhs)) }
#[inline] pub const fn wrapping_add_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).wrapping_add(get_i128(rhs)) }

#[inline] pub const fn wrapping_add_signed_u8  <  T: U8,   U: I8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).wrapping_add_signed(get_i8  (rhs)) }
#[inline] pub const fn wrapping_add_signed_u16 <  T: U16,  U: I16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).wrapping_add_signed(get_i16 (rhs)) }
#[inline] pub const fn wrapping_add_signed_u32 <  T: U32,  U: I32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).wrapping_add_signed(get_i32 (rhs)) }
#[inline] pub const fn wrapping_add_signed_u64 <  T: U64,  U: I64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).wrapping_add_signed(get_i64 (rhs)) }
#[inline] pub const fn wrapping_add_signed_u128<  T: U128, U: I128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).wrapping_add_signed(get_i128(rhs)) }
#[inline] pub const fn wrapping_add_unsigned_i8  <T: I8,   U: U8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).wrapping_add_unsigned(get_u8  (rhs)) }
#[inline] pub const fn wrapping_add_unsigned_i16 <T: I16,  U: U16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).wrapping_add_unsigned(get_u16 (rhs)) }
#[inline] pub const fn wrapping_add_unsigned_i32 <T: I32,  U: U32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).wrapping_add_unsigned(get_u32 (rhs)) }
#[inline] pub const fn wrapping_add_unsigned_i64 <T: I64,  U: U64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).wrapping_add_unsigned(get_u64 (rhs)) }
#[inline] pub const fn wrapping_add_unsigned_i128<T: I128, U: U128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).wrapping_add_unsigned(get_u128(rhs)) }

#[inline] pub const fn wrapping_sub_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).wrapping_sub(get_u8  (rhs)) }
#[inline] pub const fn wrapping_sub_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).wrapping_sub(get_u16 (rhs)) }
#[inline] pub const fn wrapping_sub_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).wrapping_sub(get_u32 (rhs)) }
#[inline] pub const fn wrapping_sub_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).wrapping_sub(get_u64 (rhs)) }
#[inline] pub const fn wrapping_sub_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).wrapping_sub(get_u128(rhs)) }
#[inline] pub const fn wrapping_sub_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).wrapping_sub(get_i8  (rhs)) }
#[inline] pub const fn wrapping_sub_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).wrapping_sub(get_i16 (rhs)) }
#[inline] pub const fn wrapping_sub_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).wrapping_sub(get_i32 (rhs)) }
#[inline] pub const fn wrapping_sub_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).wrapping_sub(get_i64 (rhs)) }
#[inline] pub const fn wrapping_sub_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).wrapping_sub(get_i128(rhs)) }

#[inline] pub const fn wrapping_sub_signed_u8  <T: U8,   U: I8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).wrapping_sub_signed(get_i8  (rhs)) }
#[inline] pub const fn wrapping_sub_signed_u16 <T: U16,  U: I16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).wrapping_sub_signed(get_i16 (rhs)) }
#[inline] pub const fn wrapping_sub_signed_u32 <T: U32,  U: I32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).wrapping_sub_signed(get_i32 (rhs)) }
#[inline] pub const fn wrapping_sub_signed_u64 <T: U64,  U: I64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).wrapping_sub_signed(get_i64 (rhs)) }
#[inline] pub const fn wrapping_sub_signed_u128<T: U128, U: I128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).wrapping_sub_signed(get_i128(rhs)) }
#[inline] pub const fn wrapping_sub_unsigned_i8  <T: I8,   U: U8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).wrapping_sub_unsigned(get_u8  (rhs)) }
#[inline] pub const fn wrapping_sub_unsigned_i16 <T: I16,  U: U16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).wrapping_sub_unsigned(get_u16 (rhs)) }
#[inline] pub const fn wrapping_sub_unsigned_i32 <T: I32,  U: U32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).wrapping_sub_unsigned(get_u32 (rhs)) }
#[inline] pub const fn wrapping_sub_unsigned_i64 <T: I64,  U: U64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).wrapping_sub_unsigned(get_u64 (rhs)) }
#[inline] pub const fn wrapping_sub_unsigned_i128<T: I128, U: U128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).wrapping_sub_unsigned(get_u128(rhs)) }

#[inline] pub const fn wrapping_neg_u8  <T: U8  >(t: T) -> u8   { get_u8  (t).wrapping_neg() }
#[inline] pub const fn wrapping_neg_u16 <T: U16 >(t: T) -> u16  { get_u16 (t).wrapping_neg() }
#[inline] pub const fn wrapping_neg_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).wrapping_neg() }
#[inline] pub const fn wrapping_neg_u64 <T: U64 >(t: T) -> u64  { get_u64 (t).wrapping_neg() }
#[inline] pub const fn wrapping_neg_u128<T: U128>(t: T) -> u128 { get_u128(t).wrapping_neg() }
#[inline] pub const fn wrapping_neg_i8  <T: I8  >(t: T) -> i8   { get_i8  (t).wrapping_neg() }
#[inline] pub const fn wrapping_neg_i16 <T: I16 >(t: T) -> i16  { get_i16 (t).wrapping_neg() }
#[inline] pub const fn wrapping_neg_i32 <T: I32 >(t: T) -> i32  { get_i32 (t).wrapping_neg() }
#[inline] pub const fn wrapping_neg_i64 <T: I64 >(t: T) -> i64  { get_i64 (t).wrapping_neg() }
#[inline] pub const fn wrapping_neg_i128<T: I128>(t: T) -> i128 { get_i128(t).wrapping_neg() }

#[inline] pub const fn wrapping_mul_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).wrapping_mul(get_u8  (rhs)) }
#[inline] pub const fn wrapping_mul_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).wrapping_mul(get_u16 (rhs)) }
#[inline] pub const fn wrapping_mul_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).wrapping_mul(get_u32 (rhs)) }
#[inline] pub const fn wrapping_mul_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).wrapping_mul(get_u64 (rhs)) }
#[inline] pub const fn wrapping_mul_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).wrapping_mul(get_u128(rhs)) }
#[inline] pub const fn wrapping_mul_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).wrapping_mul(get_i8  (rhs)) }
#[inline] pub const fn wrapping_mul_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).wrapping_mul(get_i16 (rhs)) }
#[inline] pub const fn wrapping_mul_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).wrapping_mul(get_i32 (rhs)) }
#[inline] pub const fn wrapping_mul_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).wrapping_mul(get_i64 (rhs)) }
#[inline] pub const fn wrapping_mul_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).wrapping_mul(get_i128(rhs)) }

#[inline] pub const fn wrapping_div_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).wrapping_div(get_u8  (rhs)) }
#[inline] pub const fn wrapping_div_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).wrapping_div(get_u16 (rhs)) }
#[inline] pub const fn wrapping_div_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).wrapping_div(get_u32 (rhs)) }
#[inline] pub const fn wrapping_div_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).wrapping_div(get_u64 (rhs)) }
#[inline] pub const fn wrapping_div_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).wrapping_div(get_u128(rhs)) }
#[inline] pub const fn wrapping_div_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).wrapping_div(get_i8  (rhs)) }
#[inline] pub const fn wrapping_div_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).wrapping_div(get_i16 (rhs)) }
#[inline] pub const fn wrapping_div_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).wrapping_div(get_i32 (rhs)) }
#[inline] pub const fn wrapping_div_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).wrapping_div(get_i64 (rhs)) }
#[inline] pub const fn wrapping_div_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).wrapping_div(get_i128(rhs)) }

#[inline] pub const fn wrapping_div_euclid_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).wrapping_div_euclid(get_u8  (rhs)) }
#[inline] pub const fn wrapping_div_euclid_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).wrapping_div_euclid(get_u16 (rhs)) }
#[inline] pub const fn wrapping_div_euclid_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).wrapping_div_euclid(get_u32 (rhs)) }
#[inline] pub const fn wrapping_div_euclid_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).wrapping_div_euclid(get_u64 (rhs)) }
#[inline] pub const fn wrapping_div_euclid_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).wrapping_div_euclid(get_u128(rhs)) }
#[inline] pub const fn wrapping_div_euclid_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).wrapping_div_euclid(get_i8  (rhs)) }
#[inline] pub const fn wrapping_div_euclid_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).wrapping_div_euclid(get_i16 (rhs)) }
#[inline] pub const fn wrapping_div_euclid_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).wrapping_div_euclid(get_i32 (rhs)) }
#[inline] pub const fn wrapping_div_euclid_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).wrapping_div_euclid(get_i64 (rhs)) }
#[inline] pub const fn wrapping_div_euclid_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).wrapping_div_euclid(get_i128(rhs)) }

#[inline] pub const fn wrapping_rem_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).wrapping_rem(get_u8  (rhs)) }
#[inline] pub const fn wrapping_rem_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).wrapping_rem(get_u16 (rhs)) }
#[inline] pub const fn wrapping_rem_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).wrapping_rem(get_u32 (rhs)) }
#[inline] pub const fn wrapping_rem_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).wrapping_rem(get_u64 (rhs)) }
#[inline] pub const fn wrapping_rem_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).wrapping_rem(get_u128(rhs)) }
#[inline] pub const fn wrapping_rem_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).wrapping_rem(get_i8  (rhs)) }
#[inline] pub const fn wrapping_rem_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).wrapping_rem(get_i16 (rhs)) }
#[inline] pub const fn wrapping_rem_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).wrapping_rem(get_i32 (rhs)) }
#[inline] pub const fn wrapping_rem_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).wrapping_rem(get_i64 (rhs)) }
#[inline] pub const fn wrapping_rem_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).wrapping_rem(get_i128(rhs)) }

#[inline] pub const fn wrapping_rem_euclid_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).wrapping_rem_euclid(get_u8  (rhs)) }
#[inline] pub const fn wrapping_rem_euclid_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).wrapping_rem_euclid(get_u16 (rhs)) }
#[inline] pub const fn wrapping_rem_euclid_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).wrapping_rem_euclid(get_u32 (rhs)) }
#[inline] pub const fn wrapping_rem_euclid_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).wrapping_rem_euclid(get_u64 (rhs)) }
#[inline] pub const fn wrapping_rem_euclid_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).wrapping_rem_euclid(get_u128(rhs)) }
#[inline] pub const fn wrapping_rem_euclid_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).wrapping_rem_euclid(get_i8  (rhs)) }
#[inline] pub const fn wrapping_rem_euclid_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).wrapping_rem_euclid(get_i16 (rhs)) }
#[inline] pub const fn wrapping_rem_euclid_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).wrapping_rem_euclid(get_i32 (rhs)) }
#[inline] pub const fn wrapping_rem_euclid_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).wrapping_rem_euclid(get_i64 (rhs)) }
#[inline] pub const fn wrapping_rem_euclid_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).wrapping_rem_euclid(get_i128(rhs)) }

#[inline] pub const fn wrapping_pow_u8  <T: U8  >(t: T, rhs: u32) -> u8   { get_u8  (t).wrapping_pow(rhs) }
#[inline] pub const fn wrapping_pow_u16 <T: U16 >(t: T, rhs: u32) -> u16  { get_u16 (t).wrapping_pow(rhs) }
#[inline] pub const fn wrapping_pow_u32 <T: U32 >(t: T, rhs: u32) -> u32  { get_u32 (t).wrapping_pow(rhs) }
#[inline] pub const fn wrapping_pow_u64 <T: U64 >(t: T, rhs: u32) -> u64  { get_u64 (t).wrapping_pow(rhs) }
#[inline] pub const fn wrapping_pow_u128<T: U128>(t: T, rhs: u32) -> u128 { get_u128(t).wrapping_pow(rhs) }
#[inline] pub const fn wrapping_pow_i8  <T: I8  >(t: T, rhs: u32) -> i8   { get_i8  (t).wrapping_pow(rhs) }
#[inline] pub const fn wrapping_pow_i16 <T: I16 >(t: T, rhs: u32) -> i16  { get_i16 (t).wrapping_pow(rhs) }
#[inline] pub const fn wrapping_pow_i32 <T: I32 >(t: T, rhs: u32) -> i32  { get_i32 (t).wrapping_pow(rhs) }
#[inline] pub const fn wrapping_pow_i64 <T: I64 >(t: T, rhs: u32) -> i64  { get_i64 (t).wrapping_pow(rhs) }
#[inline] pub const fn wrapping_pow_i128<T: I128>(t: T, rhs: u32) -> i128 { get_i128(t).wrapping_pow(rhs) }

#[inline] pub const fn wrapping_shl_u8  <T: U8,   N: U32>(t: T, n: N) -> u8   { get_u8  (t).wrapping_shl(get_u32(n)) }
#[inline] pub const fn wrapping_shl_u16 <T: U16,  N: U32>(t: T, n: N) -> u16  { get_u16 (t).wrapping_shl(get_u32(n)) }
#[inline] pub const fn wrapping_shl_u32 <T: U32,  N: U32>(t: T, n: N) -> u32  { get_u32 (t).wrapping_shl(get_u32(n)) }
#[inline] pub const fn wrapping_shl_u64 <T: U64,  N: U32>(t: T, n: N) -> u64  { get_u64 (t).wrapping_shl(get_u32(n)) }
#[inline] pub const fn wrapping_shl_u128<T: U128, N: U32>(t: T, n: N) -> u128 { get_u128(t).wrapping_shl(get_u32(n)) }
#[inline] pub const fn wrapping_shl_i8  <T: I8,   N: U32>(t: T, n: N) -> i8   { get_i8  (t).wrapping_shl(get_u32(n)) }
#[inline] pub const fn wrapping_shl_i16 <T: I16,  N: U32>(t: T, n: N) -> i16  { get_i16 (t).wrapping_shl(get_u32(n)) }
#[inline] pub const fn wrapping_shl_i32 <T: I32,  N: U32>(t: T, n: N) -> i32  { get_i32 (t).wrapping_shl(get_u32(n)) }
#[inline] pub const fn wrapping_shl_i64 <T: I64,  N: U32>(t: T, n: N) -> i64  { get_i64 (t).wrapping_shl(get_u32(n)) }
#[inline] pub const fn wrapping_shl_i128<T: I128, N: U32>(t: T, n: N) -> i128 { get_i128(t).wrapping_shl(get_u32(n)) }

#[inline] pub const fn wrapping_shr_u8  <T: U8,   N: U32>(t: T, n: N) -> u8   { get_u8  (t).wrapping_shr(get_u32(n)) }
#[inline] pub const fn wrapping_shr_u16 <T: U16,  N: U32>(t: T, n: N) -> u16  { get_u16 (t).wrapping_shr(get_u32(n)) }
#[inline] pub const fn wrapping_shr_u32 <T: U32,  N: U32>(t: T, n: N) -> u32  { get_u32 (t).wrapping_shr(get_u32(n)) }
#[inline] pub const fn wrapping_shr_u64 <T: U64,  N: U32>(t: T, n: N) -> u64  { get_u64 (t).wrapping_shr(get_u32(n)) }
#[inline] pub const fn wrapping_shr_u128<T: U128, N: U32>(t: T, n: N) -> u128 { get_u128(t).wrapping_shr(get_u32(n)) }
#[inline] pub const fn wrapping_shr_i8  <T: I8,   N: U32>(t: T, n: N) -> i8   { get_i8  (t).wrapping_shr(get_u32(n)) }
#[inline] pub const fn wrapping_shr_i16 <T: I16,  N: U32>(t: T, n: N) -> i16  { get_i16 (t).wrapping_shr(get_u32(n)) }
#[inline] pub const fn wrapping_shr_i32 <T: I32,  N: U32>(t: T, n: N) -> i32  { get_i32 (t).wrapping_shr(get_u32(n)) }
#[inline] pub const fn wrapping_shr_i64 <T: I64,  N: U32>(t: T, n: N) -> i64  { get_i64 (t).wrapping_shr(get_u32(n)) }
#[inline] pub const fn wrapping_shr_i128<T: I128, N: U32>(t: T, n: N) -> i128 { get_i128(t).wrapping_shr(get_u32(n)) }

// Rotate left/right

#[inline] pub const fn rotate_left_u8  <T: U8,    N: U32>(t: T, n: N) -> u8   { get_u8  (t).rotate_left(get_u32(n)) }
#[inline] pub const fn rotate_left_u16 <T: U16,   N: U32>(t: T, n: N) -> u16  { get_u16 (t).rotate_left(get_u32(n)) }
#[inline] pub const fn rotate_left_u32 <T: U32,   N: U32>(t: T, n: N) -> u32  { get_u32 (t).rotate_left(get_u32(n)) }
#[inline] pub const fn rotate_left_u64 <T: U64,   N: U32>(t: T, n: N) -> u64  { get_u64 (t).rotate_left(get_u32(n)) }
#[inline] pub const fn rotate_left_u128<T: U128,  N: U32>(t: T, n: N) -> u128 { get_u128(t).rotate_left(get_u32(n)) }
#[inline] pub const fn rotate_left_i8  <T: I8,    N: U32>(t: T, n: N) -> i8   { get_i8  (t).rotate_left(get_u32(n)) }
#[inline] pub const fn rotate_left_i16 <T: I16,   N: U32>(t: T, n: N) -> i16  { get_i16 (t).rotate_left(get_u32(n)) }
#[inline] pub const fn rotate_left_i32 <T: I32,   N: U32>(t: T, n: N) -> i32  { get_i32 (t).rotate_left(get_u32(n)) }
#[inline] pub const fn rotate_left_i64 <T: I64,   N: U32>(t: T, n: N) -> i64  { get_i64 (t).rotate_left(get_u32(n)) }
#[inline] pub const fn rotate_left_i128<T: I128,  N: U32>(t: T, n: N) -> i128 { get_i128(t).rotate_left(get_u32(n)) }

#[inline] pub const fn rotate_right_u8  <T: U8,   N: U32>(t: T, n: N) -> u8   { get_u8  (t).rotate_right(get_u32(n)) }
#[inline] pub const fn rotate_right_u16 <T: U16,  N: U32>(t: T, n: N) -> u16  { get_u16 (t).rotate_right(get_u32(n)) }
#[inline] pub const fn rotate_right_u32 <T: U32,  N: U32>(t: T, n: N) -> u32  { get_u32 (t).rotate_right(get_u32(n)) }
#[inline] pub const fn rotate_right_u64 <T: U64,  N: U32>(t: T, n: N) -> u64  { get_u64 (t).rotate_right(get_u32(n)) }
#[inline] pub const fn rotate_right_u128<T: U128, N: U32>(t: T, n: N) -> u128 { get_u128(t).rotate_right(get_u32(n)) }
#[inline] pub const fn rotate_right_i8  <T: I8,   N: U32>(t: T, n: N) -> i8   { get_i8  (t).rotate_right(get_u32(n)) }
#[inline] pub const fn rotate_right_i16 <T: I16,  N: U32>(t: T, n: N) -> i16  { get_i16 (t).rotate_right(get_u32(n)) }
#[inline] pub const fn rotate_right_i32 <T: I32,  N: U32>(t: T, n: N) -> i32  { get_i32 (t).rotate_right(get_u32(n)) }
#[inline] pub const fn rotate_right_i64 <T: I64,  N: U32>(t: T, n: N) -> i64  { get_i64 (t).rotate_right(get_u32(n)) }
#[inline] pub const fn rotate_right_i128<T: I128, N: U32>(t: T, n: N) -> i128 { get_i128(t).rotate_right(get_u32(n)) }

// From/to be/le

#[inline] pub const fn from_be_u8  <T: U8  >(t: T) -> u8   { u8  ::from_be(get_u8  (t)) }
#[inline] pub const fn from_be_u16 <T: U16 >(t: T) -> u16  { u16 ::from_be(get_u16 (t)) }
#[inline] pub const fn from_be_u32 <T: U32 >(t: T) -> u32  { u32 ::from_be(get_u32 (t)) }
#[inline] pub const fn from_be_u64 <T: U64 >(t: T) -> u64  { u64 ::from_be(get_u64 (t)) }
#[inline] pub const fn from_be_u128<T: U128>(t: T) -> u128 { u128::from_be(get_u128(t)) }
#[inline] pub const fn from_be_i8  <T: I8  >(t: T) -> i8   { i8  ::from_be(get_i8  (t)) }
#[inline] pub const fn from_be_i16 <T: I16 >(t: T) -> i16  { i16 ::from_be(get_i16 (t)) }
#[inline] pub const fn from_be_i32 <T: I32 >(t: T) -> i32  { i32 ::from_be(get_i32 (t)) }
#[inline] pub const fn from_be_i64 <T: I64 >(t: T) -> i64  { i64 ::from_be(get_i64 (t)) }
#[inline] pub const fn from_be_i128<T: I128>(t: T) -> i128 { i128::from_be(get_i128(t)) }

#[inline] pub const fn from_le_u8  <T: U8  >(t: T) -> u8   { u8  ::from_le(get_u8  (t)) }
#[inline] pub const fn from_le_u16 <T: U16 >(t: T) -> u16  { u16 ::from_le(get_u16 (t)) }
#[inline] pub const fn from_le_u32 <T: U32 >(t: T) -> u32  { u32 ::from_le(get_u32 (t)) }
#[inline] pub const fn from_le_u64 <T: U64 >(t: T) -> u64  { u64 ::from_le(get_u64 (t)) }
#[inline] pub const fn from_le_u128<T: U128>(t: T) -> u128 { u128::from_le(get_u128(t)) }
#[inline] pub const fn from_le_i8  <T: I8  >(t: T) -> i8   { i8  ::from_le(get_i8  (t)) }
#[inline] pub const fn from_le_i16 <T: I16 >(t: T) -> i16  { i16 ::from_le(get_i16 (t)) }
#[inline] pub const fn from_le_i32 <T: I32 >(t: T) -> i32  { i32 ::from_le(get_i32 (t)) }
#[inline] pub const fn from_le_i64 <T: I64 >(t: T) -> i64  { i64 ::from_le(get_i64 (t)) }
#[inline] pub const fn from_le_i128<T: I128>(t: T) -> i128 { i128::from_le(get_i128(t)) }

#[inline] pub const fn to_be_u8  <T: U8  >(t: T) -> u8   { get_u8  (t).to_be() }
#[inline] pub const fn to_be_u16 <T: U16 >(t: T) -> u16  { get_u16 (t).to_be() }
#[inline] pub const fn to_be_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).to_be() }
#[inline] pub const fn to_be_u64 <T: U64 >(t: T) -> u64  { get_u64 (t).to_be() }
#[inline] pub const fn to_be_u128<T: U128>(t: T) -> u128 { get_u128(t).to_be() }
#[inline] pub const fn to_be_i8  <T: I8  >(t: T) -> i8   { get_i8  (t).to_be() }
#[inline] pub const fn to_be_i16 <T: I16 >(t: T) -> i16  { get_i16 (t).to_be() }
#[inline] pub const fn to_be_i32 <T: I32 >(t: T) -> i32  { get_i32 (t).to_be() }
#[inline] pub const fn to_be_i64 <T: I64 >(t: T) -> i64  { get_i64 (t).to_be() }
#[inline] pub const fn to_be_i128<T: I128>(t: T) -> i128 { get_i128(t).to_be() }

#[inline] pub const fn to_le_u8  <T: U8  >(t: T) -> u8   { get_u8  (t).to_le() }
#[inline] pub const fn to_le_u16 <T: U16 >(t: T) -> u16  { get_u16 (t).to_le() }
#[inline] pub const fn to_le_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).to_le() }
#[inline] pub const fn to_le_u64 <T: U64 >(t: T) -> u64  { get_u64 (t).to_le() }
#[inline] pub const fn to_le_u128<T: U128>(t: T) -> u128 { get_u128(t).to_le() }
#[inline] pub const fn to_le_i8  <T: I8  >(t: T) -> i8   { get_i8  (t).to_le() }
#[inline] pub const fn to_le_i16 <T: I16 >(t: T) -> i16  { get_i16 (t).to_le() }
#[inline] pub const fn to_le_i32 <T: I32 >(t: T) -> i32  { get_i32 (t).to_le() }
#[inline] pub const fn to_le_i64 <T: I64 >(t: T) -> i64  { get_i64 (t).to_le() }
#[inline] pub const fn to_le_i128<T: I128>(t: T) -> i128 { get_i128(t).to_le() }

#[inline] pub const fn ilog_u8  <T: U8,   U: U8  >(t: T, base: U) -> u32 { get_u8  (t).ilog(get_u8  (base)) }
#[inline] pub const fn ilog_u16 <T: U16,  U: U16 >(t: T, base: U) -> u32 { get_u16 (t).ilog(get_u16 (base)) }
#[inline] pub const fn ilog_u32 <T: U32,  U: U32 >(t: T, base: U) -> u32 { get_u32 (t).ilog(get_u32 (base)) }
#[inline] pub const fn ilog_u64 <T: U64,  U: U64 >(t: T, base: U) -> u32 { get_u64 (t).ilog(get_u64 (base)) }
#[inline] pub const fn ilog_u128<T: U128, U: U128>(t: T, base: U) -> u32 { get_u128(t).ilog(get_u128(base)) }
#[inline] pub const fn ilog_i8  <T: I8,   U: I8  >(t: T, base: U) -> u32 { get_i8  (t).ilog(get_i8  (base)) }
#[inline] pub const fn ilog_i16 <T: I16,  U: I16 >(t: T, base: U) -> u32 { get_i16 (t).ilog(get_i16 (base)) }
#[inline] pub const fn ilog_i32 <T: I32,  U: I32 >(t: T, base: U) -> u32 { get_i32 (t).ilog(get_i32 (base)) }
#[inline] pub const fn ilog_i64 <T: I64,  U: I64 >(t: T, base: U) -> u32 { get_i64 (t).ilog(get_i64 (base)) }
#[inline] pub const fn ilog_i128<T: I128, U: I128>(t: T, base: U) -> u32 { get_i128(t).ilog(get_i128(base)) }

#[inline] pub const fn ilog2_u8  <T: U8  >(t: T) -> u32  { get_u8  (t).ilog2() }
#[inline] pub const fn ilog2_u16 <T: U16 >(t: T) -> u32  { get_u16 (t).ilog2() }
#[inline] pub const fn ilog2_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).ilog2() }
#[inline] pub const fn ilog2_u64 <T: U64 >(t: T) -> u32  { get_u64 (t).ilog2() }
#[inline] pub const fn ilog2_u128<T: U128>(t: T) -> u32  { get_u128(t).ilog2() }
#[inline] pub const fn ilog2_i8  <T: I8  >(t: T) -> u32  { get_i8  (t).ilog2() }
#[inline] pub const fn ilog2_i16 <T: I16 >(t: T) -> u32  { get_i16 (t).ilog2() }
#[inline] pub const fn ilog2_i32 <T: I32 >(t: T) -> u32  { get_i32 (t).ilog2() }
#[inline] pub const fn ilog2_i64 <T: I64 >(t: T) -> u32  { get_i64 (t).ilog2() }
#[inline] pub const fn ilog2_i128<T: I128>(t: T) -> u32  { get_i128(t).ilog2() }

#[inline] pub const fn ilog10_u8  <T: U8  >(t: T) -> u32  { get_u8  (t).ilog10() }
#[inline] pub const fn ilog10_u16 <T: U16 >(t: T) -> u32  { get_u16 (t).ilog10() }
#[inline] pub const fn ilog10_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).ilog10() }
#[inline] pub const fn ilog10_u64 <T: U64 >(t: T) -> u32  { get_u64 (t).ilog10() }
#[inline] pub const fn ilog10_u128<T: U128>(t: T) -> u32  { get_u128(t).ilog10() }
#[inline] pub const fn ilog10_i8  <T: I8  >(t: T) -> u32  { get_i8  (t).ilog10() }
#[inline] pub const fn ilog10_i16 <T: I16 >(t: T) -> u32  { get_i16 (t).ilog10() }
#[inline] pub const fn ilog10_i32 <T: I32 >(t: T) -> u32  { get_i32 (t).ilog10() }
#[inline] pub const fn ilog10_i64 <T: I64 >(t: T) -> u32  { get_i64 (t).ilog10() }
#[inline] pub const fn ilog10_i128<T: I128>(t: T) -> u32  { get_i128(t).ilog10() }

#[inline] pub const fn abs_i8  <T: I8  >(t: T) -> i8   { get_i8  (t).abs() }
#[inline] pub const fn abs_i16 <T: I16 >(t: T) -> i16  { get_i16 (t).abs() }
#[inline] pub const fn abs_i32 <T: I32 >(t: T) -> i32  { get_i32 (t).abs() }
#[inline] pub const fn abs_i64 <T: I64 >(t: T) -> i64  { get_i64 (t).abs() }
#[inline] pub const fn abs_i128<T: I128>(t: T) -> i128 { get_i128(t).abs() }

#[inline] pub const fn signum_i8  <T: I8  >(t: T) -> i8   { get_i8  (t).signum() }
#[inline] pub const fn signum_i16 <T: I16 >(t: T) -> i16  { get_i16 (t).signum() }
#[inline] pub const fn signum_i32 <T: I32 >(t: T) -> i32  { get_i32 (t).signum() }
#[inline] pub const fn signum_i64 <T: I64 >(t: T) -> i64  { get_i64 (t).signum() }
#[inline] pub const fn signum_i128<T: I128>(t: T) -> i128 { get_i128(t).signum() }

#[inline] pub const fn abs_diff_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).abs_diff(get_u8  (rhs)) }
#[inline] pub const fn abs_diff_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).abs_diff(get_u16 (rhs)) }
#[inline] pub const fn abs_diff_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).abs_diff(get_u32 (rhs)) }
#[inline] pub const fn abs_diff_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).abs_diff(get_u64 (rhs)) }
#[inline] pub const fn abs_diff_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).abs_diff(get_u128(rhs)) }
#[inline] pub const fn abs_diff_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> u8   { get_i8  (lhs).abs_diff(get_i8  (rhs)) }
#[inline] pub const fn abs_diff_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> u16  { get_i16 (lhs).abs_diff(get_i16 (rhs)) }
#[inline] pub const fn abs_diff_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> u32  { get_i32 (lhs).abs_diff(get_i32 (rhs)) }
#[inline] pub const fn abs_diff_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> u64  { get_i64 (lhs).abs_diff(get_i64 (rhs)) }
#[inline] pub const fn abs_diff_i128<T: I128, U: I128>(lhs: T, rhs: U) -> u128 { get_i128(lhs).abs_diff(get_i128(rhs)) }

#[inline] pub const fn pow_u8  <T: U8  >(t: T, exp: u32) -> u8   { get_u8  (t).pow(exp) }
#[inline] pub const fn pow_u16 <T: U16 >(t: T, exp: u32) -> u16  { get_u16 (t).pow(exp) }
#[inline] pub const fn pow_u32 <T: U32 >(t: T, exp: u32) -> u32  { get_u32 (t).pow(exp) }
#[inline] pub const fn pow_u64 <T: U64 >(t: T, exp: u32) -> u64  { get_u64 (t).pow(exp) }
#[inline] pub const fn pow_u128<T: U128>(t: T, exp: u32) -> u128 { get_u128(t).pow(exp) }
#[inline] pub const fn pow_i8  <T: I8  >(t: T, exp: u32) -> i8   { get_i8  (t).pow(exp) }
#[inline] pub const fn pow_i16 <T: I16 >(t: T, exp: u32) -> i16  { get_i16 (t).pow(exp) }
#[inline] pub const fn pow_i32 <T: I32 >(t: T, exp: u32) -> i32  { get_i32 (t).pow(exp) }
#[inline] pub const fn pow_i64 <T: I64 >(t: T, exp: u32) -> i64  { get_i64 (t).pow(exp) }
#[inline] pub const fn pow_i128<T: I128>(t: T, exp: u32) -> i128 { get_i128(t).pow(exp) }

#[inline] pub const fn isqrt_u8  <T: U8  >(t: T) -> u8   { get_u8  (t).isqrt() }
#[inline] pub const fn isqrt_u16 <T: U16 >(t: T) -> u16  { get_u16 (t).isqrt() }
#[inline] pub const fn isqrt_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).isqrt() }
#[inline] pub const fn isqrt_u64 <T: U64 >(t: T) -> u64  { get_u64 (t).isqrt() }
#[inline] pub const fn isqrt_u128<T: U128>(t: T) -> u128 { get_u128(t).isqrt() }
#[inline] pub const fn isqrt_i8  <T: I8  >(t: T) -> i8   { get_i8  (t).isqrt() }
#[inline] pub const fn isqrt_i16 <T: I16 >(t: T) -> i16  { get_i16 (t).isqrt() }
#[inline] pub const fn isqrt_i32 <T: I32 >(t: T) -> i32  { get_i32 (t).isqrt() }
#[inline] pub const fn isqrt_i64 <T: I64 >(t: T) -> i64  { get_i64 (t).isqrt() }
#[inline] pub const fn isqrt_i128<T: I128>(t: T) -> i128 { get_i128(t).isqrt() }

#[inline] pub const fn div_euclid_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).div_euclid(get_u8  (rhs)) }
#[inline] pub const fn div_euclid_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).div_euclid(get_u16 (rhs)) }
#[inline] pub const fn div_euclid_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).div_euclid(get_u32 (rhs)) }
#[inline] pub const fn div_euclid_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).div_euclid(get_u64 (rhs)) }
#[inline] pub const fn div_euclid_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).div_euclid(get_u128(rhs)) }
#[inline] pub const fn div_euclid_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).div_euclid(get_i8  (rhs)) }
#[inline] pub const fn div_euclid_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).div_euclid(get_i16 (rhs)) }
#[inline] pub const fn div_euclid_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).div_euclid(get_i32 (rhs)) }
#[inline] pub const fn div_euclid_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).div_euclid(get_i64 (rhs)) }
#[inline] pub const fn div_euclid_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).div_euclid(get_i128(rhs)) }

#[inline] pub const fn rem_euclid_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).rem_euclid(get_u8  (rhs)) }
#[inline] pub const fn rem_euclid_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).rem_euclid(get_u16 (rhs)) }
#[inline] pub const fn rem_euclid_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).rem_euclid(get_u32 (rhs)) }
#[inline] pub const fn rem_euclid_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).rem_euclid(get_u64 (rhs)) }
#[inline] pub const fn rem_euclid_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).rem_euclid(get_u128(rhs)) }
#[inline] pub const fn rem_euclid_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).rem_euclid(get_i8  (rhs)) }
#[inline] pub const fn rem_euclid_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).rem_euclid(get_i16 (rhs)) }
#[inline] pub const fn rem_euclid_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).rem_euclid(get_i32 (rhs)) }
#[inline] pub const fn rem_euclid_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).rem_euclid(get_i64 (rhs)) }
#[inline] pub const fn rem_euclid_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).rem_euclid(get_i128(rhs)) }

#[inline] pub const fn div_ceil_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).div_ceil(get_u8  (rhs)) }
#[inline] pub const fn div_ceil_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).div_ceil(get_u16 (rhs)) }
#[inline] pub const fn div_ceil_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).div_ceil(get_u32 (rhs)) }
#[inline] pub const fn div_ceil_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).div_ceil(get_u64 (rhs)) }
#[inline] pub const fn div_ceil_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).div_ceil(get_u128(rhs)) }

/* #![feature(int_roundings)] [`#88581`](https://github.com/rust-lang/rust/issues/88581)
#[inline] pub const fn div_ceil_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).div_ceil(get_i8  (rhs)) }
#[inline] pub const fn div_ceil_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).div_ceil(get_i16 (rhs)) }
#[inline] pub const fn div_ceil_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).div_ceil(get_i32 (rhs)) }
#[inline] pub const fn div_ceil_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).div_ceil(get_i64 (rhs)) }
#[inline] pub const fn div_ceil_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).div_ceil(get_i128(rhs)) }
// */

#[inline] pub const fn next_multiple_of_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).next_multiple_of(get_u8  (rhs)) }
#[inline] pub const fn next_multiple_of_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).next_multiple_of(get_u16 (rhs)) }
#[inline] pub const fn next_multiple_of_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).next_multiple_of(get_u32 (rhs)) }
#[inline] pub const fn next_multiple_of_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).next_multiple_of(get_u64 (rhs)) }
#[inline] pub const fn next_multiple_of_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).next_multiple_of(get_u128(rhs)) }

/* #![feature(int_roundings)] [`#88581`](https://github.com/rust-lang/rust/issues/88581)
#[inline] pub const fn next_multiple_of_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).next_multiple_of(get_i8  (rhs)) }
#[inline] pub const fn next_multiple_of_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).next_multiple_of(get_i16 (rhs)) }
#[inline] pub const fn next_multiple_of_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).next_multiple_of(get_i32 (rhs)) }
#[inline] pub const fn next_multiple_of_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).next_multiple_of(get_i64 (rhs)) }
#[inline] pub const fn next_multiple_of_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).next_multiple_of(get_i128(rhs)) }
// */

#[inline] pub const fn is_positive_i8  <T: I8  >(t: T) -> bool { get_i8  (t).is_positive() }
#[inline] pub const fn is_positive_i16 <T: I16 >(t: T) -> bool { get_i16 (t).is_positive() }
#[inline] pub const fn is_positive_i32 <T: I32 >(t: T) -> bool { get_i32 (t).is_positive() }
#[inline] pub const fn is_positive_i64 <T: I64 >(t: T) -> bool { get_i64 (t).is_positive() }
#[inline] pub const fn is_positive_i128<T: I128>(t: T) -> bool { get_i128(t).is_positive() }

#[inline] pub const fn is_negative_i8  <T: I8  >(t: T) -> bool { get_i8  (t).is_negative() }
#[inline] pub const fn is_negative_i16 <T: I16 >(t: T) -> bool { get_i16 (t).is_negative() }
#[inline] pub const fn is_negative_i32 <T: I32 >(t: T) -> bool { get_i32 (t).is_negative() }
#[inline] pub const fn is_negative_i64 <T: I64 >(t: T) -> bool { get_i64 (t).is_negative() }
#[inline] pub const fn is_negative_i128<T: I128>(t: T) -> bool { get_i128(t).is_negative() }

#[inline] pub const fn is_multiple_of_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> bool { get_u8  (lhs).is_multiple_of(get_u8  (rhs)) }
#[inline] pub const fn is_multiple_of_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> bool { get_u16 (lhs).is_multiple_of(get_u16 (rhs)) }
#[inline] pub const fn is_multiple_of_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> bool { get_u32 (lhs).is_multiple_of(get_u32 (rhs)) }
#[inline] pub const fn is_multiple_of_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> bool { get_u64 (lhs).is_multiple_of(get_u64 (rhs)) }
#[inline] pub const fn is_multiple_of_u128<T: U128, U: U128>(lhs: T, rhs: U) -> bool { get_u128(lhs).is_multiple_of(get_u128(rhs)) }

#[inline] pub const fn is_power_of_two_u8  <T: U8  >(t: T) -> bool { get_u8  (t).is_power_of_two() }
#[inline] pub const fn is_power_of_two_u16 <T: U16 >(t: T) -> bool { get_u16 (t).is_power_of_two() }
#[inline] pub const fn is_power_of_two_u32 <T: U32 >(t: T) -> bool { get_u32 (t).is_power_of_two() }
#[inline] pub const fn is_power_of_two_u64 <T: U64 >(t: T) -> bool { get_u64 (t).is_power_of_two() }
#[inline] pub const fn is_power_of_two_u128<T: U128>(t: T) -> bool { get_u128(t).is_power_of_two() }

#[inline] pub const fn next_power_of_two_u8  <T: U8  >(t: T) -> u8   { get_u8  (t).next_power_of_two() }
#[inline] pub const fn next_power_of_two_u16 <T: U16 >(t: T) -> u16  { get_u16 (t).next_power_of_two() }
#[inline] pub const fn next_power_of_two_u32 <T: U32 >(t: T) -> u32  { get_u32 (t).next_power_of_two() }
#[inline] pub const fn next_power_of_two_u64 <T: U64 >(t: T) -> u64  { get_u64 (t).next_power_of_two() }
#[inline] pub const fn next_power_of_two_u128<T: U128>(t: T) -> u128 { get_u128(t).next_power_of_two() }

#[inline] pub const fn from_le_bytes_u8  <T: U8  >(bytes: [u8;  1]) -> u8   { u8  ::from_le_bytes(bytes) }
#[inline] pub const fn from_le_bytes_u16 <T: U16 >(bytes: [u8;  2]) -> u16  { u16 ::from_le_bytes(bytes) }
#[inline] pub const fn from_le_bytes_u32 <T: U32 >(bytes: [u8;  4]) -> u32  { u32 ::from_le_bytes(bytes) }
#[inline] pub const fn from_le_bytes_u64 <T: U64 >(bytes: [u8;  8]) -> u64  { u64 ::from_le_bytes(bytes) }
#[inline] pub const fn from_le_bytes_u128<T: U128>(bytes: [u8; 16]) -> u128 { u128::from_le_bytes(bytes) }
#[inline] pub const fn from_le_bytes_i8  <T: I8  >(bytes: [u8;  1]) -> i8   { i8  ::from_le_bytes(bytes) }
#[inline] pub const fn from_le_bytes_i16 <T: I16 >(bytes: [u8;  2]) -> i16  { i16 ::from_le_bytes(bytes) }
#[inline] pub const fn from_le_bytes_i32 <T: I32 >(bytes: [u8;  4]) -> i32  { i32 ::from_le_bytes(bytes) }
#[inline] pub const fn from_le_bytes_i64 <T: I64 >(bytes: [u8;  8]) -> i64  { i64 ::from_le_bytes(bytes) }
#[inline] pub const fn from_le_bytes_i128<T: I128>(bytes: [u8; 16]) -> i128 { i128::from_le_bytes(bytes) }

#[inline] pub const fn from_ne_bytes_u8  <T: U8  >(bytes: [u8;  1]) -> u8   { u8  ::from_ne_bytes(bytes) }
#[inline] pub const fn from_ne_bytes_u16 <T: U16 >(bytes: [u8;  2]) -> u16  { u16 ::from_ne_bytes(bytes) }
#[inline] pub const fn from_ne_bytes_u32 <T: U32 >(bytes: [u8;  4]) -> u32  { u32 ::from_ne_bytes(bytes) }
#[inline] pub const fn from_ne_bytes_u64 <T: U64 >(bytes: [u8;  8]) -> u64  { u64 ::from_ne_bytes(bytes) }
#[inline] pub const fn from_ne_bytes_u128<T: U128>(bytes: [u8; 16]) -> u128 { u128::from_ne_bytes(bytes) }
#[inline] pub const fn from_ne_bytes_i8  <T: I8  >(bytes: [u8;  1]) -> i8   { i8  ::from_ne_bytes(bytes) }
#[inline] pub const fn from_ne_bytes_i16 <T: I16 >(bytes: [u8;  2]) -> i16  { i16 ::from_ne_bytes(bytes) }
#[inline] pub const fn from_ne_bytes_i32 <T: I32 >(bytes: [u8;  4]) -> i32  { i32 ::from_ne_bytes(bytes) }
#[inline] pub const fn from_ne_bytes_i64 <T: I64 >(bytes: [u8;  8]) -> i64  { i64 ::from_ne_bytes(bytes) }
#[inline] pub const fn from_ne_bytes_i128<T: I128>(bytes: [u8; 16]) -> i128 { i128::from_ne_bytes(bytes) }

#[inline] pub const fn midpoint_u8  <T: U8,   U: U8  >(lhs: T, rhs: U) -> u8   { get_u8  (lhs).midpoint(get_u8  (rhs)) }
#[inline] pub const fn midpoint_u16 <T: U16,  U: U16 >(lhs: T, rhs: U) -> u16  { get_u16 (lhs).midpoint(get_u16 (rhs)) }
#[inline] pub const fn midpoint_u32 <T: U32,  U: U32 >(lhs: T, rhs: U) -> u32  { get_u32 (lhs).midpoint(get_u32 (rhs)) }
#[inline] pub const fn midpoint_u64 <T: U64,  U: U64 >(lhs: T, rhs: U) -> u64  { get_u64 (lhs).midpoint(get_u64 (rhs)) }
#[inline] pub const fn midpoint_u128<T: U128, U: U128>(lhs: T, rhs: U) -> u128 { get_u128(lhs).midpoint(get_u128(rhs)) }
#[inline] pub const fn midpoint_i8  <T: I8,   U: I8  >(lhs: T, rhs: U) -> i8   { get_i8  (lhs).midpoint(get_i8  (rhs)) }
#[inline] pub const fn midpoint_i16 <T: I16,  U: I16 >(lhs: T, rhs: U) -> i16  { get_i16 (lhs).midpoint(get_i16 (rhs)) }
#[inline] pub const fn midpoint_i32 <T: I32,  U: I32 >(lhs: T, rhs: U) -> i32  { get_i32 (lhs).midpoint(get_i32 (rhs)) }
#[inline] pub const fn midpoint_i64 <T: I64,  U: I64 >(lhs: T, rhs: U) -> i64  { get_i64 (lhs).midpoint(get_i64 (rhs)) }
#[inline] pub const fn midpoint_i128<T: I128, U: I128>(lhs: T, rhs: U) -> i128 { get_i128(lhs).midpoint(get_i128(rhs)) }

#[inline] pub const fn is_ascii_u8             <T: U8>(t: T) -> bool { get_u8(t).is_ascii() }
#[inline] pub const fn is_ascii_alphabetic_u8  <T: U8>(t: T) -> bool { get_u8(t).is_ascii_alphabetic() }
#[inline] pub const fn is_ascii_uppercase_u8   <T: U8>(t: T) -> bool { get_u8(t).is_ascii_uppercase() }
#[inline] pub const fn is_ascii_lowercase_u8   <T: U8>(t: T) -> bool { get_u8(t).is_ascii_lowercase() }
#[inline] pub const fn is_ascii_alphanumeric_u8<T: U8>(t: T) -> bool { get_u8(t).is_ascii_alphanumeric() }
#[inline] pub const fn is_ascii_digit_u8       <T: U8>(t: T) -> bool { get_u8(t).is_ascii_digit() }
#[inline] pub const fn is_ascii_hexdigit_u8    <T: U8>(t: T) -> bool { get_u8(t).is_ascii_hexdigit() }
#[inline] pub const fn is_ascii_punctuation_u8 <T: U8>(t: T) -> bool { get_u8(t).is_ascii_punctuation() }
#[inline] pub const fn is_ascii_graphic_u8     <T: U8>(t: T) -> bool { get_u8(t).is_ascii_graphic() }
#[inline] pub const fn is_ascii_whitespace_u8  <T: U8>(t: T) -> bool { get_u8(t).is_ascii_whitespace() }
#[inline] pub const fn is_ascii_control_u8     <T: U8>(t: T) -> bool { get_u8(t).is_ascii_control() }
#[inline] pub const fn to_ascii_uppercase_u8   <T: U8>(t: T) -> u8   { get_u8(t).to_ascii_uppercase() }
#[inline] pub const fn to_ascii_lowercase_u8   <T: U8>(t: T) -> u8   { get_u8(t).to_ascii_lowercase() }

#[inline] pub const fn eq_ignore_ascii_case<
    T: U8,
    U: U8
>(lhs: T, rhs: U) -> bool {
    get_u8(lhs).eq_ignore_ascii_case(&get_u8(rhs))
}

#[cfg(test)]
mod tests;
