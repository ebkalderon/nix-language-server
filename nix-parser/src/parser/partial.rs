//! Data structures and parser combinators for working with partial values.

use std::iter::FromIterator;

use codespan::Span;
use nom::bytes::complete::take;
use nom::combinator::opt;
use nom::sequence::preceded;
use nom::InputLength;

use super::{tokens, IResult};
use crate::error::{Error, Errors};
use crate::lexer::Tokens;
use crate::ToSpan;

/// A partial value which might contain errors.
///
/// `Partial<T>` is a data structure which may or may not contain a value, and also may or may not
/// contain errors. It is essentially an [`std::option::Option`] with an associated [`Errors`]
/// stack.
///
/// [`std::option::Option`]: https://doc.rust-lang.org/std/option/enum.Option.html
/// [`Errors`]: ../error/struct.Errors.html
///
/// This type is used to accumulate and apply monadic transformations to a possibly incomplete
/// [`Expr`] or [`SourceFile`]. Consumers of `Partial<T>` values can choose to assert that the
/// contained value exists without errors with [`Partial::verify()`], which will consume the
/// `Partial<T>` and transform it into a `Result<T, Errors>` which can be handled normally.
///
/// [`Expr`]: ../ast/enum.Expr.html
/// [`SourceFile`]: ../ast/enum.SourceFile.html
/// [`Partial::verify()`]: #method.verify
///
/// # Examples
///
/// ```
/// use codespan::Span;
/// use nix_parser::error::ExpectedFoundError;
/// use nix_parser::parser::Partial;
///
/// let mut one = Partial::new(Some(1));
/// assert!(one.value().is_some());
/// assert!(!one.has_errors());
///
/// let checked = if one.value().filter(|i| **i >= 0).is_some() {
///     one.map(|value| value + 1)
/// } else {
///     let error = ExpectedFoundError::new("positive number", "negative number", Span::initial());
///     one.extend_errors(std::iter::once(error.into()));
///     one
/// };
///
/// // You can inspect the partial value and enumerate the errors.
/// println!("Errors, if any: {:?}", checked.errors());
/// if let Some(ref partial) = checked.value() {
///     println!("Partial value is: {}", partial);
/// } else {
///     println!("No value found");
/// }
///
/// // Or you can assert that the contained value exists and has no errors.
/// assert_eq!(checked.verify(), Ok(2));
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct Partial<T> {
    value: Option<T>,
    errors: Errors,
}

impl<T> Partial<T> {
    /// Constructs a new `Partial<T>` with the given initial value.
    ///
    /// # Examples
    ///
    /// ```
    /// # use nix_parser::parser::Partial;
    /// let something: Partial<u32> = Partial::new(Some(1));
    /// let nothing: Partial<u32> = Partial::from(None);
    /// ```
    #[inline]
    pub fn new(value: Option<T>) -> Self {
        Partial {
            value,
            errors: Errors::new(),
        }
    }

    /// Constructs a new `Partial<T>` with the given initial value and a stack of errors.
    ///
    /// # Examples
    ///
    /// ```
    /// # use nix_parser::parser::Partial;
    /// # use nix_parser::error::{Errors, UnexpectedError};
    /// use codespan::Span;
    ///
    /// let mut errors = Errors::new();
    /// errors.push(UnexpectedError::new("token", Span::new(3, 4)));
    ///
    /// let value = Partial::with_errors(Some(1), errors);
    /// ```
    #[inline]
    pub fn with_errors(value: Option<T>, errors: Errors) -> Self {
        Partial { value, errors }
    }

    /// Returns whether this partial value contains errors.
    ///
    /// # Examples
    ///
    /// ```
    /// # use nix_parser::parser::Partial;
    /// let value = Partial::from("example");
    /// assert!(!value.has_errors());
    ///
    /// // The line above is equivalent to:
    /// assert!(value.errors().is_empty());
    /// ```
    #[inline]
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Returns a reference to any errors associated with the partial value.
    ///
    /// # Examples
    ///
    /// ```
    /// # use nix_parser::parser::Partial;
    /// let value = Partial::from("example");
    /// assert_eq!(value.errors().len(), 0);
    /// ```
    #[inline]
    pub fn errors(&self) -> &Errors {
        &self.errors
    }

    /// Appends the given error to the error stack contained in this partial value.
    ///
    /// # Examples
    ///
    /// ```
    /// # use nix_parser::parser::Partial;
    /// use codespan::Span;
    /// use nix_parser::error::Error;
    ///
    /// let mut partial = Partial::from("example");
    /// assert!(!partial.has_errors());
    ///
    /// let first = Error::Message(Span::new(1, 3), "oops".into());
    /// let second = Error::Message(Span::new(5, 7), "sorry".into());
    /// partial.extend_errors(vec![first, second]);
    /// assert_eq!(partial.errors().len(), 2);
    /// ```
    pub fn extend_errors<I: IntoIterator<Item = Error>>(&mut self, error: I) {
        self.errors.extend(error);
    }

    /// Returns the contained partial value, if any.
    ///
    /// # Examples
    ///
    /// ```
    /// # use nix_parser::parser::Partial;
    /// let nothing: Partial<u32> = Partial::from(None);
    /// assert_eq!(nothing.value(), None);
    ///
    /// let something: Partial<u32> = Partial::from(1);
    /// assert_eq!(something.value(), Some(&1));
    /// ```
    #[inline]
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
    /// # use nix_parser::error::Errors;
    /// # use nix_parser::parser::Partial;
    /// let value = Partial::from("Hello, world!");
    /// let length = value.map(|s| s.len());
    /// assert_eq!(length.value(), Some(&13));
    /// ```
    #[inline]
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
    ///
    /// # Examples
    ///
    /// Notice in the example below how both the values and errors are accumulated.
    ///
    /// ```
    /// # use nix_parser::error::{Errors, UnexpectedError};
    /// # use nix_parser::parser::Partial;
    /// use codespan::Span;
    ///
    /// let one = Partial::from(1u32);
    ///
    /// let mut errors = Errors::new();
    /// errors.push(UnexpectedError::new("token", Span::new(3, 4)));
    /// let two = Partial::with_errors(Some(2u32), errors);
    ///
    /// let three = one.flat_map(|x| two.map(|y| x + y));
    ///
    /// assert_eq!(three.value(), Some(&3));
    /// assert_eq!(three.errors().len(), 1);
    /// ```
    ///
    /// If any partial value in the chain returns a `None`, the final value will be `None`.
    /// However, the errors are always accumulated regardless.
    ///
    /// ```
    /// # use nix_parser::error::{Errors, ExpectedFoundError};
    /// # use nix_parser::parser::Partial;
    /// use codespan::Span;
    ///
    /// let one = Partial::from(1u32);
    ///
    /// let mut errors = Errors::new();
    /// errors.push(ExpectedFoundError::new("foo", "bar", Span::new(5, 5)));
    /// let two: Partial<u32> = Partial::with_errors(None, errors);
    ///
    /// let three = Partial::from(3u32);
    ///
    /// let four = one.flat_map(|x| two.flat_map(|y| three.map(|z| x + y + z)));
    ///
    /// assert_eq!(four.value(), None);
    /// assert_eq!(four.errors().len(), 1);
    /// ```
    #[inline]
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

    /// Transforms the `Partial<T>` into a `Result<T, Errors>`, asserting that the contained value
    /// exists and has no errors.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nix_parser::parser::Partial;
    /// let partial = Partial::from(123);
    /// assert_eq!(partial.verify(), Ok(123));
    ///
    /// let partial: Partial<u32> = Partial::from(None);
    /// assert!(partial.verify().is_err());
    /// ```
    #[inline]
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

        if let Some(values) = self.value.as_mut() {
            let (lower, upper) = iter.size_hint();
            values.reserve(upper.unwrap_or(lower).saturating_sub(values.capacity()));
        }

        for partial in iter {
            self.extend_errors(partial.errors);

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
        let (lower, upper) = iter.size_hint();
        let init = (Vec::with_capacity(upper.unwrap_or(lower)), Errors::new());
        let (values, errors) = iter.fold(init, |(mut values, mut errors), partial| {
            values.extend(partial.value);
            errors.extend(partial.errors);
            (values, errors)
        });

        Partial::with_errors(Some(values), errors)
    }
}

/// Combinator which runs the given partial parser and then expects on a terminator.
///
/// If the terminator is missing, an unclosed delimiter error will be appended to the `Partial`,
/// and parsing will be allowed to continue as though the terminator existed.
pub fn expect_terminated<'a, O1, O2, F, G>(
    f: F,
    term: G,
) -> impl Fn(Tokens<'a>) -> IResult<Partial<O1>>
where
    F: Fn(Tokens<'a>) -> IResult<Partial<O1>>,
    G: Fn(Tokens<'a>) -> IResult<O2>,
{
    move |input| {
        let (remaining, mut partial) = f(input)?;
        match term(remaining) {
            Ok((remaining, _)) => Ok((remaining, partial)),
            Err(nom::Err::Error(err)) => {
                partial.extend_errors(err);
                Ok((remaining, partial))
            }
            Err(err) => Err(err),
        }
    }
}

/// Combinator which behaves like `nom::combinator::map`, except it is a shorthand for:
///
/// ```rust,ignore
/// map(partial, |partial| partial.map(&f))
/// ```
pub fn map_partial<'a, O1, O2, P, F>(
    partial: P,
    f: F,
) -> impl Fn(Tokens<'a>) -> IResult<Partial<O2>>
where
    P: Fn(Tokens<'a>) -> IResult<Partial<O1>>,
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
) -> impl Fn(Tokens<'a>) -> IResult<Partial<O2>>
where
    P: Fn(Tokens<'a>) -> IResult<Partial<O1>>,
    F: Fn(Span, O1) -> O2,
{
    move |input| {
        let (remainder, partial) = partial(input)?;
        let span = if remainder.input_len() > 0 {
            Span::new(input.to_span().start(), remainder.to_span().start())
        } else {
            input.to_span()
        };
        Ok((remainder, partial.map(|p| f(span, p))))
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
) -> impl Fn(Tokens<'a>) -> IResult<Partial<Vec<O1>>>
where
    F: Fn(Tokens<'a>) -> IResult<Partial<O1>>,
    G: Fn(Tokens<'a>) -> IResult<O2>,
{
    move |input| {
        let mut partials = Vec::new();
        let mut errors = Errors::new();
        let mut input = input;

        loop {
            match g(input) {
                Ok(_) => {
                    let mut partial: Partial<_> = partials.into_iter().collect();
                    partial.extend_errors(errors);
                    return Ok((input, partial));
                }
                Err(nom::Err::Failure(_)) | Err(nom::Err::Error(_)) => match f(input) {
                    Err(nom::Err::Failure(err)) | Err(nom::Err::Error(err)) => {
                        if tokens::eof(input).is_ok() {
                            let partial: Partial<_> = partials.into_iter().collect();
                            return Ok((input, partial));
                        } else if let Ok((remainder, _)) = take::<_, _, Errors>(1usize)(input) {
                            errors.extend(err);
                            input = remainder;
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

/// Combinator which returns `None` if the given partial parser fails.
///
/// This combinator behaves like `nom::combinator::opt`, except it also transposes the result from
/// `Option<Partial<O>>` to `Partial<Option<O>>`. This transposition allows you to utilize the
/// behavior of `opt` while remaining compatible with other partial combinators like
/// `pair_partial()` and `map_partial()`.
///
/// # Examples
///
/// `opt_partial()` is especially handy when combined with `pair_partial()` to create partial
/// parsers with predicates that trigger based on some non-partial condition. For example, compare
/// this regular `nom` parser with its partial equivalent:
///
/// ```rust,ignore
/// // Regular version
/// pair(f, opt(preceded(sep, g)))
///
/// // Partial version
/// pair_partial(f, opt_partial(preceded(sep, g)))
/// ```
pub fn opt_partial<'a, O, F>(f: F) -> impl Fn(Tokens<'a>) -> IResult<Partial<Option<O>>>
where
    F: Fn(Tokens<'a>) -> IResult<Partial<O>>,
{
    move |input| {
        let (remaining, value) = opt(&f)(input)?;
        match value {
            Some(partial) => Ok((remaining, partial.map(Some))),
            None => Ok((remaining, Partial::new(Some(None)))),
        }
    }
}

/// Combinator which gets the result from the first partial parser, then gets the result from the
/// second partial parser, and produces a partial value containing a tuple of the two results.
///
/// This is effectively shorthand for:
///
/// ```rust,ignore
/// map(pair(first, second), |(f, g)| f.flat_map(|f| g.map(|g| (f, g))))
/// ```
pub fn pair_partial<'a, O1, O2, F, G>(
    first: F,
    second: G,
) -> impl Fn(Tokens<'a>) -> IResult<Partial<(O1, O2)>>
where
    F: Fn(Tokens<'a>) -> IResult<Partial<O1>>,
    G: Fn(Tokens<'a>) -> IResult<Partial<O2>>,
{
    move |input| {
        let (input, f) = first(input)?;
        let (remaining, g) = second(input)?;
        Ok((remaining, f.flat_map(|f| g.map(|g| (f, g)))))
    }
}

/// Combinator which produces a list of partial elements `f` separated by parser `sep`.
///
/// This parser behaves like `nom::multi::separated_list`, except that it expects some terminator
/// `term` at the end of the list so it knows when to soft-bail.
///
/// If the terminator is missing, an unclosed delimiter error will be appended to the `Partial`,
/// and parsing will be allowed to continue as through the terminator existed.
///
/// This parser is essentially shorthand for:
///
/// ```rust,ignore
/// let (remaining, (first, rest)) = pair(&f, many_till_partial(preceded(sep, &f), term))(input)?;
/// let partial = first.flat_map(|f| rest.map(|r| std::iter::once(f).chain(r).collect()));
/// ```
pub fn separated_list_partial<'a, O1, O2, O3, F, G, H>(
    sep: G,
    term: H,
    f: F,
) -> impl Fn(Tokens<'a>) -> IResult<Partial<Vec<O1>>>
where
    F: Fn(Tokens<'a>) -> IResult<Partial<O1>>,
    G: Fn(Tokens<'a>) -> IResult<O2>,
    H: Fn(Tokens<'a>) -> IResult<O3>,
{
    move |input| {
        let mut partials = Vec::new();
        let mut errors = Errors::new();
        let mut input = input;

        match f(input) {
            Err(nom::Err::Error(_)) => return Ok((input, partials.into_iter().collect())),
            Err(nom::Err::Failure(err)) => {
                return Err(nom::Err::Error(err));
            }
            Err(err) => return Err(err),
            Ok((remaining, partial)) => {
                input = remaining;
                partials.push(partial);
            }
        }

        loop {
            match term(input) {
                Ok(_) => {
                    let mut partial: Partial<_> = partials.into_iter().collect();
                    partial.extend_errors(errors);
                    return Ok((input, partial));
                }
                Err(nom::Err::Failure(_)) | Err(nom::Err::Error(_)) => {
                    match preceded(&sep, &f)(input) {
                        Err(nom::Err::Failure(err)) | Err(nom::Err::Error(err)) => {
                            if tokens::eof(input).is_ok() {
                                let partial: Partial<_> = partials.into_iter().collect();
                                return Ok((input, partial));
                            } else if let Ok((remainder, _)) = take::<_, _, Errors>(1usize)(input) {
                                errors.extend(err);
                                input = remainder;
                            }
                        }
                        Err(err) => return Err(err),
                        Ok((remainder, elem)) => {
                            partials.push(elem);
                            input = remainder;
                        }
                    }
                }
                Err(err) => return Err(err),
            }
        }
    }
}

/// Combinator which asserts that a given partial parser produces a value and contains no errors.
pub fn verify_full<'a, O, F>(f: F) -> impl Fn(Tokens<'a>) -> IResult<O>
where
    F: Fn(Tokens<'a>) -> IResult<Partial<O>>,
{
    move |input| {
        let (input, partial) = f(input)?;
        partial
            .verify()
            .map(move |value| (input, value))
            .map_err(nom::Err::Error)
    }
}
