//! Custom iterators used by ink! IR types, abstractions and utilities.

/// An iterator where each successive item is taken from a wrapped iterator
/// which is computed from a source and a step expression.
pub struct IterSuccessors<
    T,                                      // The associated type of this iterator.
    S, // The source type to which the step expression is applied to generate the iterated items.
    I: Iterator<Item = T>, // The type of the wrapped iterator on which next is called.
    F: FnMut(&S) -> Option<(Option<I>, S)>, // The type of the step expression function.
> {
    /// The wrapped iterator.
    next_iter: Option<I>,
    /// The source input for computing the next iterator.
    next_source: S,
    /// The function applied at each step to generate the next iterator and source.
    step_expression: F,
}

impl<T, S, I, F> IterSuccessors<T, S, I, F>
where
    I: Iterator<Item = T>,
    F: FnMut(&S) -> Option<(Option<I>, S)>,
{
    /// Creates an iterator where each successive item is taken from a wrapped iterator
    /// which is computed from a source and a step expression.
    pub fn new(source: S, step_expression: F) -> Self {
        Self {
            next_iter: None,
            next_source: source,
            step_expression,
        }
    }
}

impl<T, S, I, F> Iterator for IterSuccessors<T, S, I, F>
where
    I: Iterator<Item = T>,
    F: FnMut(&S) -> Option<(Option<I>, S)>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        // Return the next item in the wrapped iterator if any.
        if let Some(next_iter) = &mut self.next_iter {
            if let Some(next_item) = next_iter.next() {
                return Some(next_item);
            }
        }

        // Recurse if the step expression either returns the next iterator or the next source.
        if let Some((next_iter, next_source)) = (self.step_expression)(&self.next_source) {
            self.next_iter = next_iter;
            self.next_source = next_source;
            return self.next();
        }

        None
    }
}
