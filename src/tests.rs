// Copyright 2025 Marsh J. Ray
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![deny(elided_lifetimes_in_paths)]
#![deny(clippy::panic, clippy::unwrap_used, clippy::expect_used, clippy::manual_assert)]

use crate::*;

#[cfg(test)]
#[allow(dead_code)]
mod t {
    use super::*;
    use static_assertions::{
        const_assert_eq,
        assert_impl_all, assert_impl_one,
        assert_type_eq_all,
    };

    #[rustfmt::skip]
    macro_rules! mt {
        // Main entry
        { } => {
            // Invoke ourselves again, but with $types_info.
            with_types_info! { mt { @mod_t_content } }
        };

        { @mod_t_content $types_info:tt } => {
            mt! { @test_fns $types_info $types_info }
        };

        { @test_fns {
                KWPIU: [ $( ( $kwpiu_tn:ident, $kwpiu_bits:tt, $kwpiu_bits_log2:tt, $kwpiu_testfn_name:ident $(,)? ) ),* $(,)? ],
                KWPIS: [ $( ( $kwpis_tn:ident, $kwpis_bits:tt, $kwpis_bits_log2:tt, $kwpis_testfn_name:ident $(,)? ) ),* $(,)? ],
                has_core_num_NonZero: $has_core_num_NonZero:tt
                $( , $extra_k:ident : $extra_v:tt )* $(,)?
            }
            $types_info:tt
        } => {
            $( mt! { @testfn $kwpiu_testfn_name Unsigned $kwpiu_bits $kwpiu_tn $kwpiu_bits_log2 $has_core_num_NonZero } )*
            $( mt! { @testfn $kwpis_testfn_name Signed   $kwpis_bits $kwpis_tn $kwpis_bits_log2 $has_core_num_NonZero } )*
        };

        { @testfn
            $testfn_name:ident
            $us:ident
            $bits:tt
            $type_name:ident
            $bits_log2:tt
            $has_core_num_NonZero:tt
        } => {
            mt! { @testfn_emit
                $testfn_name
                ( $us $bits $type_name $bits_log2 $has_core_num_NonZero ) // "details"
            }
        };

        { @testfn_emit $testfn_name:ident $details:tt } => {
            #[test]
            fn $testfn_name() {
                mt! { @testfn_body         $details $details }
                mt! { @testfn_body_us      $details $details }
                mt! { @testfn_body_exactly $details $details }
            }
        };

        { @testfn_body
            ( $us:ident $bits:tt $type_name:ident $bits_log2:tt $($rest:tt)* )
            $details:tt
        } => {
            assert_impl_all!($type_name : Integer);
            assert_impl_one!(
                $type_name: UnsignedInteger, SignedInteger );

            assert_type_eq_all!(
                < $type_name as Integer >::Type,
                $type_name );

            const_assert_eq!($type_name ::BITS_LOG2, $bits_log2);

            const EXPECTED_BITS: u64 = 1u64 << $bits_log2;
            const_assert_eq!($type_name ::BITS as u64, EXPECTED_BITS);
            const_assert_eq!($type_name ::BITS as u64, (size_of::< $type_name >()*8) as u64);
        };

        //---------------------------------------------------------------------- specialization on (Unsigned|Signed)

        { @testfn_body_us
            ( Unsigned $bits:tt $type_name:ident $bits_log2:tt $($rest:tt)* )
            $details:tt
        } => {
            assert_impl_all!($type_name : UnsignedInteger);

            const EXPECTED_MIN: $type_name = 0;
            const EXPECTED_MAX: $type_name = !EXPECTED_MIN;
            const_assert_eq!($type_name ::MIN, EXPECTED_MIN);
            const_assert_eq!($type_name ::MAX, EXPECTED_MAX);
        };

        { @testfn_body_us
            ( Signed $bits:tt $type_name:ident $bits_log2:tt $($rest:tt)* )
            $details:tt
        } => {
            assert_impl_all!($type_name : SignedInteger);

            assert_impl_all!(       $type_name :  ::core::ops::Neg                       );
            assert_type_eq_all!(<   $type_name as ::core::ops::Neg >::Output, $type_name );
            //assert_impl_all!(     & $type_name :  ::core::ops::Neg                       );
            //assert_type_eq_all!(< & $type_name as ::core::ops::Neg >::Output, $type_name );

            const EXPECTED_MIN: $type_name = (1 as $type_name) << (EXPECTED_BITS - 1);
            const EXPECTED_MAX: $type_name = -(EXPECTED_MIN + 1);
            const_assert_eq!($type_name ::MIN, EXPECTED_MIN);
            const_assert_eq!($type_name ::MAX, EXPECTED_MAX);
        };

        //------------------------------------------------------------- specialization on (Unsigned|Signed) and Bits

        //--------------------------- Unsigned

        { @testfn_body_exactly ( Unsigned 8 $type_name:ident $($rest:tt)* ) $details:tt } => {
            const _: fn() = || {
                const fn f<T>()
                where
                    T: Copy + Clone + Send + Sync + Unpin +
                        ::core::cmp::Eq + ::core::cmp::Ord +
                        ::core::fmt::Debug + ::core::fmt::Display + ::core::fmt::Octal +
                        ::core::fmt::LowerExp + ::core::fmt::UpperExp +
                        ::core::fmt::LowerHex + ::core::fmt::UpperHex +
                        ::core::hash::Hash +
                        ::core::panic::UnwindSafe + ::core::panic::RefUnwindSafe +
                        Into   <   u8> + TryInto<   i8> +
                        Into   <  u16> + Into   <  i16> +
                        Into   <  u32> + Into   <  i32> +
                        Into   <  u64> + Into   <  i64> +
                        Into   < u128> + Into   < i128> +
                        Into   <usize> + Into   <isize> +
                        From   <   u8> + TryFrom<   i8> +
                        TryFrom<  u16> + TryFrom<  i16> +
                        TryFrom<  u32> + TryFrom<  i32> +
                        TryFrom<  u64> + TryFrom<  i64> +
                        TryFrom< u128> + TryFrom< i128> +
                        TryFrom<usize> + TryFrom<isize>,
                { }
                const fn f2<T: U8>() { f::<T>(); }
                f2::<$type_name>()
            };
            mt! { @testfn_body_nz U8 $details $details }
        };

        { @testfn_body_exactly ( Unsigned 16 $type_name:ident $($rest:tt)* ) $details:tt } => {
            const _: fn() = || {
                const fn f<T>()
                    where
                    T: Copy + Clone + Send + Sync + Unpin +
                        ::core::cmp::Eq + ::core::cmp::Ord +
                        ::core::fmt::Debug + ::core::fmt::Display + ::core::fmt::Octal +
                        ::core::fmt::LowerExp + ::core::fmt::UpperExp +
                        ::core::fmt::LowerHex + ::core::fmt::UpperHex +
                        ::core::hash::Hash +
                        ::core::panic::UnwindSafe + ::core::panic::RefUnwindSafe +
                        TryInto<   u8> + TryInto<   i8> +
                        Into   <  u16> + TryInto<  i16> +
                        Into   <  u32> + Into   <  i32> +
                        Into   <  u64> + Into   <  i64> +
                        Into   < u128> + Into   < i128> +
                        Into   <usize> + TryInto<isize> +
                        From   <   u8> + TryFrom<   i8> +
                        From   <  u16> + TryFrom<  i16> +
                        TryFrom<  u32> + TryFrom<  i32> +
                        TryFrom<  u64> + TryFrom<  i64> +
                        TryFrom< u128> + TryFrom< i128> +
                        TryFrom<usize> + TryFrom<isize>,
                { }
                const fn f2<T: U16>() { f::<T>(); }
                f2::<$type_name>()
            };
            mt! { @testfn_body_nz U16 $details $details }
        };

        { @testfn_body_exactly ( Unsigned 32 $type_name:ident $($rest:tt)* ) $details:tt } => {
            const _: fn() = || {
                const fn f<T>()
                where
                    T: Copy + Clone + Send + Sync + Unpin +
                        ::core::cmp::Eq + ::core::cmp::Ord +
                        ::core::fmt::Debug + ::core::fmt::Display + ::core::fmt::Octal +
                        ::core::fmt::LowerExp + ::core::fmt::UpperExp +
                        ::core::fmt::LowerHex + ::core::fmt::UpperHex +
                        ::core::hash::Hash +
                        ::core::panic::UnwindSafe + ::core::panic::RefUnwindSafe +
                        TryInto<   u8> + TryInto<   i8> +
                        TryInto<  u16> + TryInto<  i16> +
                        Into   <  u32> + TryInto<  i32> +
                        Into   <  u64> + Into   <  i64> +
                        Into   < u128> + Into   < i128> +
                        TryInto<usize> + TryInto<isize> +
                        TryInto< char> +
                        From   <   u8> + TryFrom<   i8> +
                        From   <  u16> + TryFrom<  i16> +
                        From   <  u32> + TryFrom<  i32> +
                        TryFrom<  u64> + TryFrom<  i64> +
                        TryFrom< u128> + TryFrom< i128> +
                        TryFrom<usize> + TryFrom<isize>,
                { }
                const fn f2<T: U32>() { f::<T>(); }
                f2::<$type_name>()
            };
            mt! { @testfn_body_nz U32 $details $details }
        };

        { @testfn_body_exactly ( Unsigned 64 $type_name:ident $($rest:tt)* ) $details:tt } => {
            const _: fn() = || {
                const fn f<T>() where T:
                    Copy + Clone + Send + Sync + Unpin +
                    ::core::cmp::Eq + ::core::cmp::Ord +
                    ::core::fmt::Debug + ::core::fmt::Display + ::core::fmt::Octal +
                    ::core::fmt::LowerExp + ::core::fmt::UpperExp +
                    ::core::fmt::LowerHex + ::core::fmt::UpperHex +
                    ::core::hash::Hash +
                    ::core::panic::UnwindSafe + ::core::panic::RefUnwindSafe +
                    TryInto<   u8> + TryInto<   i8> +
                    TryInto<  u16> + TryInto<  i16> +
                    TryInto<  u32> + TryInto<  i32> +
                    Into   <  u64> + TryInto<  i64> +
                    Into   < u128> + Into   < i128> +
                    TryInto<usize> + TryInto<isize> +
                    From   <   u8> + TryFrom<   i8> +
                    From   <  u16> + TryFrom<  i16> +
                    From   <  u32> + TryFrom<  i32> +
                    From   <  u64> + TryFrom<  i64> +
                    TryFrom< u128> + TryFrom< i128> +
                    TryFrom<usize> + TryFrom<isize>,
                { }
                const fn f2<T: U64>() { f::<T>(); }
                f2::<$type_name>()
            };
            mt! { @testfn_body_nz U64 $details $details }
        };

        { @testfn_body_exactly ( Unsigned 128 $type_name:ident $($rest:tt)* ) $details:tt } => {
            const _: fn() = || {
                const fn f<T>() where T:
                    Copy + Clone + Send + Sync + Unpin +
                    ::core::cmp::Eq + ::core::cmp::Ord +
                    ::core::fmt::Debug + ::core::fmt::Display + ::core::fmt::Octal +
                    ::core::fmt::LowerExp + ::core::fmt::UpperExp +
                    ::core::fmt::LowerHex + ::core::fmt::UpperHex +
                    ::core::hash::Hash +
                    ::core::panic::UnwindSafe + ::core::panic::RefUnwindSafe +
                    TryInto<   u8> + TryInto<   i8> +
                    TryInto<  u16> + TryInto<  i16> +
                    TryInto<  u32> + TryInto<  i32> +
                    TryInto<  u64> + TryInto<  i64> +
                    TryInto<usize> + TryInto<isize> +
                    Into   < u128> + TryInto< i128> +
                    From   <   u8> + TryFrom<   i8> +
                    From   <  u16> + TryFrom<  i16> +
                    From   <  u32> + TryFrom<  i32> +
                    From   <  u64> + TryFrom<  i64> +
                    From   < u128> + TryFrom< i128> +
                    TryFrom<usize> + TryFrom<isize>,
                { }
                const fn f2<T: U128>() { f::<T>(); }
                f2::<$type_name>()
            };
            mt! { @testfn_body_nz U128 $details $details }
        };

        //--------------------------- Signed

        { @testfn_body_exactly ( Signed 8 $type_name:ident $($rest:tt)* ) $details:tt } => {
            const _: fn() = || {
                const fn f<T>()
                where
                    T: Copy + Clone + Send + Sync + Unpin +
                        ::core::cmp::Eq + ::core::cmp::Ord +
                        ::core::fmt::Debug + ::core::fmt::Display + ::core::fmt::Octal +
                        ::core::fmt::LowerExp + ::core::fmt::UpperExp +
                        ::core::fmt::LowerHex + ::core::fmt::UpperHex +
                        ::core::hash::Hash +
                        ::core::panic::UnwindSafe + ::core::panic::RefUnwindSafe +
                        TryInto<   u8> + Into   <   i8> +
                        TryInto<  u16> + Into   <  i16> +
                        TryInto<  u32> + Into   <  i32> +
                        TryInto<  u64> + Into   <  i64> +
                        TryInto< u128> + Into   < i128> +
                        TryInto<usize> + Into   <isize> +
                        TryFrom<   u8> + From   <   i8> +
                        TryFrom<  u16> + TryFrom<  i16> +
                        TryFrom<  u32> + TryFrom<  i32> +
                        TryFrom<  u64> + TryFrom<  i64> +
                        TryFrom< u128> + TryFrom< i128> +
                        TryFrom<usize> + TryFrom<isize>,
                    //isize: From<T>
                { }
                const fn f2<T: I8>() { f::<T>(); }
                f2::<$type_name>()
            };
            mt! { @testfn_body_nz I8 $details $details }
        };

        { @testfn_body_exactly ( Signed 16 $type_name:ident $($rest:tt)* ) $details:tt } => {
            const _: fn() = || {
                const fn f<T>()
                where
                    T: Copy + Clone + Send + Sync + Unpin +
                        ::core::cmp::Eq + ::core::cmp::Ord +
                        ::core::fmt::Debug + ::core::fmt::Display + ::core::fmt::Octal +
                        ::core::fmt::LowerExp + ::core::fmt::UpperExp +
                        ::core::fmt::LowerHex + ::core::fmt::UpperHex +
                        ::core::hash::Hash +
                        ::core::panic::UnwindSafe + ::core::panic::RefUnwindSafe +
                        TryInto<   u8> + TryInto<   i8> +
                        TryInto<  u16> + Into   <  i16> +
                        TryInto<  u32> + Into   <  i32> +
                        TryInto<  u64> + Into   <  i64> +
                        TryInto< u128> + Into   < i128> +
                        TryInto<usize> + Into   <isize> +
                        From   <   u8> + From   <   i8> +
                        TryFrom<  u16> + From   <  i16> +
                        TryFrom<  u32> + TryFrom<  i32> +
                        TryFrom<  u64> + TryFrom<  i64> +
                        TryFrom< u128> + TryFrom< i128> +
                        TryFrom<usize> + TryFrom<isize>,
                    //isize: From<T>
                { }
                const fn f2<T: I16>() { f::<T>(); }
                f2::<$type_name>()
            };
            mt! { @testfn_body_nz I16 $details $details }
        };

        { @testfn_body_exactly ( Signed 32 $type_name:ident $($rest:tt)* ) $details:tt } => {
            const _: fn() = || {
                const fn f<T>() where T:
                    Copy + Clone + Send + Sync + Unpin +
                    ::core::cmp::Eq + ::core::cmp::Ord +
                    ::core::fmt::Debug + ::core::fmt::Display + ::core::fmt::Octal +
                    ::core::fmt::LowerExp + ::core::fmt::UpperExp +
                    ::core::fmt::LowerHex + ::core::fmt::UpperHex +
                    ::core::hash::Hash +
                    ::core::panic::UnwindSafe + ::core::panic::RefUnwindSafe +
                    TryInto<   u8> + TryInto<   i8> +
                    TryInto<  u16> + TryInto<  i16> +
                    TryInto<  u32> + Into   <  i32> +
                    TryInto<  u64> + Into   <  i64> +
                    TryInto< u128> + Into   < i128> +
                    TryInto<usize> + TryInto<isize> +
                    From   <   u8> + From   <   i8> +
                    From   <  u16> + From   <  i16> +
                    TryFrom<  u32> + From   <  i32> +
                    TryFrom<  u64> + TryFrom<  i64> +
                    TryFrom< u128> + TryFrom< i128> +
                    TryFrom<usize> + TryFrom<isize>,
                { }
                const fn f2<T: I32>() { f::<T>(); }
                f2::<$type_name>()
            };
            mt! { @testfn_body_nz I32 $details $details }
        };

        { @testfn_body_exactly ( Signed 64 $type_name:ident $($rest:tt)* ) $details:tt } => {
            const _: fn() = || {
                const fn f<T>() where T:
                    Copy + Clone + Send + Sync + Unpin +
                    ::core::cmp::Eq + ::core::cmp::Ord +
                    ::core::fmt::Debug + ::core::fmt::Display + ::core::fmt::Octal +
                    ::core::fmt::LowerExp + ::core::fmt::UpperExp +
                    ::core::fmt::LowerHex + ::core::fmt::UpperHex +
                    ::core::hash::Hash +
                    ::core::panic::UnwindSafe + ::core::panic::RefUnwindSafe +
                    TryInto<   u8> + TryInto<   i8> +
                    TryInto<  u16> + TryInto<  i16> +
                    TryInto<  u32> + TryInto<  i32> +
                    TryInto<  u64> + Into   <  i64> +
                    TryInto< u128> + Into   < i128> +
                    TryInto<usize> + TryInto<isize> +
                    From   <   u8> + From   <   i8> +
                    From   <  u16> + From   <  i16> +
                    From   <  u32> + From   <  i32> +
                    TryFrom<  u64> + From   <  i64> +
                    TryFrom< u128> + TryFrom< i128> +
                    TryFrom<usize> + TryFrom<isize>,
                { }
                const fn f2<T: I64>() { f::<T>(); }
                f2::<$type_name>()
            };
            mt! { @testfn_body_nz I64 $details $details }
        };

        { @testfn_body_exactly ( Signed 128 $type_name:ident $($rest:tt)* ) $details:tt } => {
            const _: fn() = || {
                const fn f<T>() where T:
                    Copy + Clone + Send + Sync + Unpin +
                    ::core::cmp::Eq + ::core::cmp::Ord +
                    ::core::fmt::Debug + ::core::fmt::Display + ::core::fmt::Octal +
                    ::core::fmt::LowerExp + ::core::fmt::UpperExp +
                    ::core::fmt::LowerHex + ::core::fmt::UpperHex +
                    ::core::hash::Hash +
                    ::core::panic::UnwindSafe + ::core::panic::RefUnwindSafe +
                    TryInto<   u8> + TryInto<   i8> +
                    TryInto<  u16> + TryInto<  i16> +
                    TryInto<  u32> + TryInto<  i32> +
                    TryInto<  u64> + TryInto<  i64> +
                    TryInto< u128> + Into   < i128> +
                    TryInto<usize> + TryInto<isize> +
                    From   <   u8> + From   <   i8> +
                    From   <  u16> + From   <  i16> +
                    From   <  u32> + From   <  i32> +
                    From   <  u64> + From   <  i64> +
                    TryFrom< u128> + From   < i128> +
                    TryFrom<usize> + TryFrom<isize>,
                { }
                const fn f2<T: I128>() { f::<T>(); }
                f2::<$type_name>()
            };
            mt! { @testfn_body_nz I128 $details $details }
        };

        //------------------------------------------------------- specialization on has_core_num_NonZero

        { @testfn_body_nz
            $exactly_trait:ident
            ( $us:ident $bits:tt $type_name:ident $bits_log2:tt $has_core_num_NonZero:tt $($rest:tt)* )
            $details:tt
        } => {
            mt! { @testfn_body_nz2 $has_core_num_NonZero $exactly_trait $details }
        };

        { @testfn_body_nz2 yes $exactly_trait:ident $details:tt } => {
            mt! { @testfn_body_nz3 $exactly_trait $details $details }
        };
        { @testfn_body_nz2 $($extra:tt)* } => { /* has_core_num_NonZero != yes */ };

        { @testfn_body_nz3 $exactly_trait:ident
            ( $us:ident $bits:tt $type_name:ident $($rest:tt)* )
            $details:tt
        } => {
            const _: fn() = || {
                use ::core::num::NonZero;
                const fn f2<T>()
                where T:
                    From<NonZero< $type_name >> +
                    TryInto<NonZero< $type_name >> +
                    //?TODO: ::core::ops::DivAssign<NonZero<$type_name>> +
                    //?TODO: ::core::ops::RemAssign<NonZero<$type_name>> +
                    //?TODO: ::core::ops::Div<NonZero<$type_name>> +
                    //?TODO: ::core::ops::Rem<NonZero<$type_name>> +
                    //?TODO: ::core::ops::BitOr<NonZero<$type_name>> +
                    Copy
                { }
                const fn f1<T: $exactly_trait>() {
                    f2::<T>();
                }
                f1::<$type_name>();
            };
        };
    }

    mt! {  }
}
