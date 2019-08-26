use std::iter::FromIterator;

use codespan::Span;
use nom::character::complete::anychar;
use nom::combinator::{cut, recognize};
use nom::error::{ErrorKind, VerboseError, VerboseErrorKind};
use nom::multi::many_till;
use nom::sequence::terminated;
use nom::Slice;

use super::error::{Error, Errors};
use super::{IResult, LocatedSpan};
use crate::ToSpan;

#[derive(Clone, Debug, PartialEq)]
pub struct Partial<T> {
    value: Option<T>,
    errors: Errors,
}

impl<T> Partial<T> {
    /// Constructs a new `Partial<T>` with the given initial value.
    pub fn new(value: Option<T>) -> Self {
        Partial {
            value,
            errors: Errors::new(),
        }
    }

    /// Constructs a new `Partial<T>` with the given initial value and a stack of errors.
    pub fn with_errors(value: Option<T>, errors: Errors) -> Self {
        Partial { value, errors }
    }

    /// Returns whether this partial value contains errors.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Returns the errors associated with the partial value, if any.
    pub fn errors(&self) -> Option<Errors> {
        if self.has_errors() {
            Some(self.errors.clone())
        } else {
            None
        }
    }

    /// Appends the given error to the error stack contained in this partial value.
    pub fn extend_errors<I: IntoIterator<Item = Error>>(&mut self, error: I) {
        self.errors.extend(error);
    }

    /// Returns the contained partial value, if any.
    pub fn value(&self) -> Option<&T> {
        self.value.as_ref()
    }

    /// Maps a `Partial<T>` to `Partial<U>` by applying a function to a contained value.
    ///
    /// This transformation is applied regardless of whether this `Partial<T>` contains errors.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nix_parser::parser::Partial;
    /// # use nom::error::VerboseError;
    /// # use nom_locate::LocatedSpan;
    /// # fn main() -> Result<(), VerboseError<LocatedSpan<&'static str>>> {
    /// let partial_string = Partial::from(String::from("Hello, world!"));
    /// let partial_len = partial_string.map(|s| s.len());
    /// // We assert here that the contained partial value has no errors.
    /// let full_len = partial_len.verify()?;
    ///
    /// assert_eq!(full_len, 13);
    /// # Ok(())
    /// # }
    /// ```
    pub fn map<U, F>(self, f: F) -> Partial<U>
    where
        F: FnOnce(T) -> U,
    {
        Partial {
            value: self.value.map(f),
            errors: self.errors,
        }
    }

    /// Calls `f` if there exists a contained value, otherwise returns the stored errors instead.
    ///
    /// Any errors produced by `f` are appended to the errors already inside `self`.
    pub fn flat_map<U, F>(mut self, f: F) -> Partial<U>
    where
        F: FnOnce(T) -> Partial<U>,
    {
        if let Some(value) = self.value {
            let mut partial = f(value);
            self.errors.extend(partial.errors);
            partial.errors = self.errors;
            partial
        } else {
            Partial::with_errors(None, self.errors)
        }
    }

    pub fn map_err<F>(self, f: F) -> Partial<T>
    where
        F: FnOnce(Errors) -> Errors,
    {
        let errors = if self.has_errors() {
            f(self.errors)
        } else {
            self.errors
        };

        Partial {
            value: self.value,
            errors,
        }
    }

    /// Transforms the `Partial<T>` into a `Result<T, VerboseError<LocatedSpan>>`, asserting that
    /// the contained value exists and has no errors.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nix_parser::parser::Partial;
    /// # use nom::error::VerboseError;
    /// # use nom_locate::LocatedSpan;
    /// # fn main() -> Result<(), VerboseError<LocatedSpan<&'static str>>> {
    /// let partial = Partial::new(Some(123));
    /// assert_eq!(Ok(123), partial.verify());
    ///
    /// let partial: Partial<u32> = Partial::new(None);
    /// assert!(partial.verify().is_err());
    /// # Ok(())
    /// # }
    /// ```
    pub fn verify(self) -> Result<T, Errors> {
        match self.value {
            Some(_) if self.has_errors() => Err(self.errors),
            Some(value) => Ok(value),
            None => Err(self.errors),
        }
    }
}

/// Extend the contents of a `Partial<Vec<T>>` from an iterator of `Partial<T>`.
impl<T> Extend<Partial<T>> for Partial<Vec<T>> {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Partial<T>>,
    {
        let iter = iter.into_iter();

        if let (Some(values), (_, Some(bound))) = (self.value.as_mut(), iter.size_hint()) {
            let additional = bound.saturating_sub(values.len());
            values.reserve(additional);
        }

        for partial in iter {
            if let Some(errors) = partial.errors() {
                self.extend_errors(errors);
            }

            if let (Some(values), Some(value)) = (self.value.as_mut(), partial.value) {
                values.push(value);
            }
        }
    }
}

impl<T> From<T> for Partial<T> {
    fn from(value: T) -> Self {
        Partial::new(Some(value))
    }
}

impl<T> From<Option<T>> for Partial<T> {
    fn from(value: Option<T>) -> Self {
        Partial::new(value)
    }
}

/// Collect an iterator of `Partial<T>` into a `Partial<Vec<T>>`.
impl<T> FromIterator<Partial<T>> for Partial<Vec<T>> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Partial<T>>,
    {
        let iter = iter.into_iter();

        let (_, capacity) = iter.size_hint();
        let mut values = Vec::with_capacity(capacity.unwrap_or(0));
        let mut partials = Partial::new(None);

        for partial in iter {
            if let Some(errors) = partial.errors() {
                partials.extend_errors(errors);
            }

            if let Some(value) = partial.value {
                values.push(value);
            }
        }

        partials.value = Some(values);
        partials
    }
}

/// Combinator which runs the given partial parser and then expects on a terminator.
///
/// If the terminator is missing, an unclosed delimiter error will be appended to the `Partial`,
/// and parsing will be allowed to continue as through the terminator existed.
pub fn expect_terminated<'a, O1, O2, F, G>(
    f: F,
    term: G,
) -> impl Fn(LocatedSpan<'a>) -> IResult<Partial<O1>>
where
    F: Fn(LocatedSpan<'a>) -> IResult<Partial<O1>>,
    G: Fn(LocatedSpan<'a>) -> IResult<O2>,
{
    move |input| match terminated(&f, cut(&term))(input) {
        Ok((remaining, partial)) => Ok((remaining, partial)),
        Err(nom::Err::Error(err)) => {
            let (remaining, mut partial) = f(input)?;
            partial.extend_errors(err);
            Ok((remaining, partial))
        }
        Err(err) => Err(err),
    }
}

/// Combinator which behaves like `nom::combinator::map()`, except it is a shorthand for:
///
/// ```rust,ignore
/// map(partial, |partial| partial.map(&f))
/// ```
pub fn map_partial<'a, O1, O2, P, F>(
    partial: P,
    f: F,
) -> impl Fn(LocatedSpan<'a>) -> IResult<Partial<O2>>
where
    P: Fn(LocatedSpan<'a>) -> IResult<Partial<O1>>,
    F: Fn(O1) -> O2,
{
    move |input| {
        let (input, partial) = partial(input)?;
        Ok((input, partial.map(&f)))
    }
}

/// Combinator which combines the functionality of `map_partial()` and `map_spanned()`.
///
/// This is like `map_partial()` except it also includes a `Span` based on the consumed input.
pub fn map_partial_spanned<'a, O1, O2, P, F>(
    partial: P,
    f: F,
) -> impl Fn(LocatedSpan<'a>) -> IResult<Partial<O2>>
where
    P: Fn(LocatedSpan<'a>) -> IResult<Partial<O1>>,
    F: Fn(Span, O1) -> O2,
{
    move |input| {
        let (remainder, partial) = partial(input)?;
        let partial_len = remainder.offset - input.offset;
        let span = input.slice(..partial_len).to_span();
        Ok((remainder, partial.map(|p| f(span, p))))
    }
}

/// Combinator for handling fallback cases for partial parsers.
///
/// If the given partial parser succeeds, parsing continues like normal. If it fails, this
/// combinator skips up to and including the location described by `skip_to` and appends `error` to
/// the partial value.
///
/// This combinator is useful for handling fallback cases where `partial` was given a totally
/// invalid expression which it cannot recover from.
pub fn map_err_partial<'a, O1, O2, F, G>(
    partial: F,
    skip_to: G,
) -> impl Fn(LocatedSpan<'a>) -> IResult<Partial<O1>>
where
    F: Fn(LocatedSpan<'a>) -> IResult<Partial<O1>>,
    G: Fn(LocatedSpan<'a>) -> IResult<O2>,
{
    move |input| match partial(input) {
        Ok((remaining, value)) => Ok((remaining, value)),
        Err(nom::Err::Failure(e)) | Err(nom::Err::Error(e)) => {
            let (remaining, failed) = recognize(many_till(anychar, &skip_to))(input)?;
            let partial = Partial::with_errors(None, e);
            Ok((remaining, partial))
        }
        Err(e) => Err(e),
    }
}

/// Combinator which applies the partial parser `f` until the parser `g` produces a result,
/// returning a `Partial<Vec<_>>` of the results of `f`.
///
/// If the terminator is missing, an unclosed delimiter error will be appended to the `Partial`,
/// and parsing will be allowed to continue as through the terminator existed.
pub fn many_till_partial<'a, O1, O2, F, G>(
    f: F,
    g: G,
) -> impl Fn(LocatedSpan<'a>) -> IResult<Partial<Vec<O1>>>
where
    F: Fn(LocatedSpan<'a>) -> IResult<Partial<O1>>,
    G: Fn(LocatedSpan<'a>) -> IResult<O2>,
{
    move |input| {
        let mut partials = Vec::new();
        let mut errors = Errors::new();
        let mut input = input.clone();

        loop {
            match g(input) {
                Ok((_, _)) => {
                    let mut partial: Partial<_> = partials.into_iter().collect();
                    partial.extend_errors(errors);
                    return Ok((input, partial));
                }
                Err(nom::Err::Error(_)) => match f(input) {
                    Err(nom::Err::Failure(err)) | Err(nom::Err::Error(err)) => {
                        if let Ok((remainder, _)) = anychar::<_, Errors>(input) {
                            errors.extend(err);
                            input = remainder;
                        } else {
                            let partial: Partial<_> = partials.into_iter().collect();
                            let eof = input.slice(input.fragment.len()..input.fragment.len());
                            return Ok((eof, partial));
                        }
                    }
                    Err(err) => return Err(err),
                    Ok((remainder, elem)) => {
                        partials.push(elem);
                        input = remainder;
                    }
                },
                Err(err) => return Err(err),
            }
        }
    }
}

/// Combinator which asserts that a given partial parser produces a value and contains no errors.
pub fn verify_full<'a, O, F>(f: F) -> impl Fn(LocatedSpan<'a>) -> IResult<O>
where
    F: Fn(LocatedSpan<'a>) -> IResult<Partial<O>>,
{
    move |input| {
        let (input, partial) = f(input)?;
        partial
            .verify()
            .map(move |value| (input, value))
            .map_err(nom::Err::Failure)
    }
}
