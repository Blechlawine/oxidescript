use std::iter::Enumerate;

use nom::Input;

use super::token::Token;

#[derive(Clone, Copy, Debug)]
pub struct Tokens<'src> {
    pub tokens: &'src [Token<'src>],
    pub start: usize,
    pub end: usize,
}

impl<'src> Tokens<'src> {
    pub fn new(tokens: &'src [Token]) -> Self {
        Self {
            tokens,
            start: 0,
            end: tokens.len(),
        }
    }
}

impl<'src> Input for Tokens<'src> {
    type Item = &'src Token<'src>;
    type Iter = ::std::slice::Iter<'src, Token<'src>>;
    type IterIndices = Enumerate<::std::slice::Iter<'src, Token<'src>>>;

    fn iter_indices(&self) -> Self::IterIndices {
        self.tokens.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::Iter {
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

    fn input_len(&self) -> usize {
        self.tokens.len()
    }

    fn take_from(&self, index: usize) -> Self {
        Self {
            tokens: &self.tokens[index..],
            start: 0,
            end: self.tokens.len() - index,
        }
    }
}
