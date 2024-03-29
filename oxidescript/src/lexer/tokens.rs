use std::iter::Enumerate;

use nom::{InputIter, InputLength, InputTake};

use super::token::Token;

#[derive(Clone, Copy, Debug)]
pub struct Tokens<'a> {
    pub tokens: &'a [Token],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            start: 0,
            end: tokens.len(),
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token;
    type Iter = Enumerate<::std::slice::Iter<'a, Token>>;
    type IterElem = ::std::slice::Iter<'a, Token>;

    fn iter_indices(&self) -> Self::Iter {
        self.tokens.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.tokens.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        // println!("Tokens.position");
        self.tokens.iter().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        // println!("Tokens.slice_index: {}", &count);
        if self.tokens.len() >= count {
            Ok(count)
        } else {
            Err(nom::Needed::Unknown)
        }
    }
}

impl<'a> InputTake for Tokens<'a> {
    fn take(&self, count: usize) -> Self {
        Self {
            tokens: &self.tokens[0..count],
            start: 0,
            end: count,
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (left, right) = self.tokens.split_at(count);
        // println!(
        //     "Tokens.take_split: split at {} in {}: ({}|{})",
        //     &count,
        //     self.tokens.len(),
        //     left.len(),
        //     right.len()
        // );
        (
            Self {
                tokens: right,
                start: 0,
                end: right.len(),
            },
            Self {
                tokens: left,
                start: 0,
                end: left.len(),
            },
        )
    }
}

impl InputLength for Tokens<'_> {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}
