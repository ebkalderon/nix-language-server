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
    pub fn new(value: Option<T>) -> Self {
        Partial {
            value,
            errors: VerboseError { errors: Vec::new() },
        }
    }

    pub fn with_errors(value: Option<T>, errors: VerboseError<Span<'a>>) -> Self {
        Partial { value, errors }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.errors.is_empty()
    }

    pub fn errors(&self) -> Option<VerboseError<Span<'a>>> {
        if self.has_errors() {
            Some(self.errors.clone())
        } else {
            None
        }
    }

    pub fn extend_errors(&mut self, error: VerboseError<Span<'a>>) {
        self.errors.errors.extend(error.errors);
    }

    pub fn value(&self) -> Option<&T> {
        self.value.as_ref()
    }

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

    pub fn verify(self) -> Result<T, VerboseError<Span<'a>>> {
        match self.value {
            Some(_) if self.has_errors() => Err(self.errors),
            Some(value) => Ok(value),
            None => Err(self.errors),
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
            .map(move |v| (input, v))
            .map_err(nom::Err::Failure)
    }
}
