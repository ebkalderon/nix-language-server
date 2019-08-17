use std::iter::FromIterator;

use nom::bytes::complete::{tag, take_until};
use nom::combinator::{all_consuming, map, recognize};
use nom::error::VerboseError;
use nom::multi::{many0, many1};
use nom::sequence::pair;

use super::{IResult, Span};

#[derive(Clone, Debug, PartialEq)]
pub struct Partial<'a, T> {
    value: Option<T>,
    errors: VerboseError<Span<'a>>,
}

impl<'a, T> Partial<'a, T> {
    /// Constructs a new `Partial<T>` with the given initial value.
    pub fn new(value: Option<T>) -> Self {
        Partial {
            value,
            errors: VerboseError { errors: Vec::new() },
        }
    }

    /// Constructs a new `Partial<T>` with the given initial value and a stack of errors.
    pub fn with_errors(value: Option<T>, errors: VerboseError<Span<'a>>) -> Self {
        Partial { value, errors }
    }

    /// Returns whether this partial value contains errors.
    pub fn has_errors(&self) -> bool {
        !self.errors.errors.is_empty()
    }

    /// Returns the errors associated with the partial value, if any.
    pub fn errors(&self) -> Option<VerboseError<Span<'a>>> {
        if self.has_errors() {
            Some(self.errors.clone())
        } else {
            None
        }
    }

    /// Appends the given error to the error stack contained in this partial value.
    pub fn extend_errors(&mut self, error: VerboseError<Span<'a>>) {
        self.errors.errors.extend(error.errors);
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
    pub fn map<U, F>(self, f: F) -> Partial<'a, U>
    where
        F: FnOnce(T) -> U,
    {
        Partial {
            value: self.value.map(f),
            errors: self.errors,
        }
    }

    pub fn map_err<F>(self, f: F) -> Partial<'a, T>
    where
        F: FnOnce(VerboseError<Span<'a>>) -> VerboseError<Span<'a>>,
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

    /// Transforms the `Partial<T>` into a `Result<T, VerboseError<Span>>`, asserting that the
    /// contained value exists and has no errors.
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
    pub fn verify(self) -> Result<T, VerboseError<Span<'a>>> {
        match self.value {
            Some(_) if self.has_errors() => Err(self.errors),
            Some(value) => Ok(value),
            None => Err(self.errors),
        }
    }
}

/// Extend the contents of a `Partial<Vec<T>>` from an iterator of `Partial<T>`.
impl<'a, T> Extend<Partial<'a, T>> for Partial<'a, Vec<T>> {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Partial<'a, T>>,
    {
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

impl<'a, T> From<T> for Partial<'a, T> {
    fn from(value: T) -> Self {
        Partial::new(Some(value))
    }
}

impl<'a, T> From<Option<T>> for Partial<'a, T> {
    fn from(value: Option<T>) -> Self {
        Partial::new(value)
    }
}

/// Collect an iterator of `Partial<T>` into a `Partial<Vec<T>>`.
impl<'a, T> FromIterator<Partial<'a, T>> for Partial<'a, Vec<T>> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Partial<'a, T>>,
    {
        let mut values = Vec::new();
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

/// FIXME: Poorly tested, not sure if behaves as expected. Looking to turn this into a partial
/// parser that comsumes until `EOF`, and `partial_until()` is simply a wrapper which restricts the
/// input until a certain character is detected or given parser combinator succeeds.
pub fn partial<'a, F, O>(f: F) -> impl Fn(Span<'a>) -> IResult<Partial<O>>
where
    F: Fn(Span<'a>) -> IResult<O>,
{
    move |input| match f(input) {
        Ok((input, value)) => Ok((input, Partial::new(Some(value)))),
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            Ok((input, Partial::with_errors(None, e)))
        }
        Err(err) => Err(err),
    }
}

pub fn partial_until<'a, F, O>(sep: &'a str, f: F) -> impl Fn(Span<'a>) -> IResult<Partial<O>>
where
    F: Fn(Span<'a>) -> IResult<O>,
{
    move |input| {
        let (remaining, input) = recognize(pair(take_until(sep), tag(sep)))(input)?;
        match f(input) {
            Ok((_, value)) => Ok((remaining, Partial::new(Some(value)))),
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                Ok((remaining, Partial::with_errors(None, e)))
            }
            Err(err) => Err(err),
        }
    }
}

pub fn map_partial<'a, O1, O2, P, F>(partial: P, f: F) -> impl Fn(Span<'a>) -> IResult<Partial<O2>>
where
    P: Fn(Span<'a>) -> IResult<Partial<O1>>,
    F: Fn(O1) -> O2,
{
    move |input| {
        let (input, partial) = partial(input)?;
        Ok((input, partial.map(&f)))
    }
}

pub fn many0_partial<'a, O, F>(sep: &'a str, f: F) -> impl Fn(Span<'a>) -> IResult<Partial<Vec<O>>>
where
    F: Fn(Span<'a>) -> IResult<O>,
{
    move |input| map(many0(partial_until(sep, &f)), Partial::from_iter)(input)
}

pub fn many1_partial<'a, O, F>(sep: &'a str, f: F) -> impl Fn(Span<'a>) -> IResult<Partial<Vec<O>>>
where
    F: Fn(Span<'a>) -> IResult<O>,
{
    move |input| map(many1(partial_until(sep, &f)), Partial::from_iter)(input)
}

pub fn verify_full<'a, O, F>(f: F) -> impl Fn(Span<'a>) -> IResult<O>
where
    F: Fn(Span<'a>) -> IResult<Partial<O>>,
{
    move |input| {
        let (input, partial) = f(input)?;
        partial
            .verify()
            .map(move |value| (input, value))
            .map_err(nom::Err::Failure)
    }
}
