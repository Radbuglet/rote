use std::fmt;

// === UnsizedVec === //

pub struct UnsizedVec<T: ?Sized> {
    alloc: bumpalo::Bump,
    values: Vec<*mut T>,
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for UnsizedVec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T: ?Sized> UnsizedVec<T> {
    pub fn push<V>(&mut self, value: V, unsizer: impl FnOnce(&mut V) -> &mut T) {
        let value = self.alloc.alloc(value);
        let value = unsizer(value);

        // Safety: Because we are afforded exclusive access over `T` for as long as
        // `V` lives, it doesn't matter whether it's a subfield or even a completely
        // unrelated address because any argument for exclusive access needed to drop
        // `V` (but not deallocate!) can transitively apply to `T`.
        self.values.push(value);
    }

    // pub fn try_get(&self, index: usize) -> Option<&T> {
    //     self.values.get(index).map(|&v| unsafe { &*v })
    // }
    //
    // pub fn try_get_mut(&mut self, index: usize) -> Option<&mut T> {
    //     self.values.get_mut(index).map(|&mut v| unsafe { &mut *v })
    // }
    //
    // pub fn get(&self, index: usize) -> &T {
    //     unsafe { &*self.values[index] }
    // }
    //
    // pub fn get_mut(&mut self, index: usize) -> &mut T {
    //     unsafe { &mut *self.values[index] }
    // }

    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = &'a T> + 'a {
        self.values.iter().map(|&v| unsafe { &*v })
    }

    // pub fn iter_mut<'a>(&'a mut self) -> impl ExactSizeIterator<Item = &'a mut T> + 'a {
    //     self.values.iter_mut().map(|&mut v| unsafe { &mut *v })
    // }

    pub fn clear(&mut self) {
        for value in self.values.drain(..) {
            unsafe { value.drop_in_place() };
        }
        self.alloc.reset();
    }
}

impl<T: ?Sized> Default for UnsizedVec<T> {
    fn default() -> Self {
        Self {
            alloc: Default::default(),
            values: Default::default(),
        }
    }
}

impl<T: ?Sized> Drop for UnsizedVec<T> {
    fn drop(&mut self) {
        self.clear();
    }
}

// === Error === //

pub fn unwrap_display<T, E: fmt::Display>(value: Result<T, E>) -> T {
    match value {
        Ok(value) => value,
        Err(err) => panic!("{err}"),
    }
}

// === Formatting === //

// pub fn format_closure(f: impl Fn(&mut fmt::Formatter) -> fmt::Result) -> String {
//     format!("{}", FormatterFn(f))
// }

#[derive(Debug, Copy, Clone)]
pub struct FormatterFn<F>(pub F);

impl<F: Fn(&mut fmt::Formatter) -> fmt::Result> fmt::Display for FormatterFn<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.0)(f)
    }
}

#[doc(hidden)]
pub mod formatter_fn_internals {
    pub use std::{fmt::Formatter, write};
}

macro_rules! lazy_format {
    ($($tt:tt)*) => {
        $crate::util::FormatterFn(move |f: &mut $crate::util::formatter_fn_internals::Formatter|
            $crate::util::formatter_fn_internals::write!(f, $($tt)*)
        )
    };
}

pub(crate) use lazy_format;

#[derive(Debug)]
pub struct FmtRepeat<T>(pub T, pub usize);

impl<T: fmt::Display> fmt::Display for FmtRepeat<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&FmtIter::new((0..self.1).map(|_| &self.0)), f)
    }
}

#[derive(Debug)]
pub struct FmtIter<I>(pub I);

impl<I> FmtIter<I> {
    pub fn new(iter: impl IntoIterator<IntoIter = I>) -> Self {
        Self(iter.into_iter())
    }
}

impl<I> fmt::Display for FmtIter<I>
where
    I: Clone + Iterator,
    I::Item: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in self.0.clone() {
            fmt::Display::fmt(&i, f)?;
        }
        Ok(())
    }
}

// // === `overload!` macro === //
//
// macro_rules! overload {
//     (
//         $vis:vis impl $curr_name:ident {
//             $($curr_inner:tt)*
//         }
//     ) => {
//         #[derive(Debug, Copy, Clone, Default)]
//         $vis struct $curr_name;
//
//         impl $curr_name {
//             $($curr_inner)*
//         }
//     };
//     (
//         $vis:vis impl $curr_name:ident {
//             $($curr_inner:tt)*
//         }
//
//         impl $next_name:ident {
//             $($next_inner:tt)*
//         }
//
//         $($rest:tt)*
//     ) => {
//         #[derive(Debug, Copy, Clone, Default)]
//         $vis struct $curr_name;
//
//         impl $curr_name {
//             $($curr_inner)*
//         }
//
//         impl $crate::util::overload_inner::Deref for $curr_name {
//             type Target = $next_name;
//
//             fn deref(&self) -> &Self::Target {
//                 &$next_name
//             }
//         }
//
//         $crate::util::overload! {
//             $vis impl $next_name {
//                 $($next_inner)*
//             }
//
//             $($rest)*
//         }
//     };
// }
//
// pub(crate) use overload;
//
// #[doc(hidden)]
// pub mod overload_inner {
//     pub use std::ops::Deref;
// }
