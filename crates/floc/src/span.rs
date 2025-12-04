#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    #[inline]
    pub fn contains(&self, offset: usize) -> bool {
        self.start <= offset && offset <= self.end
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl<'a> From<pest::Span<'a>> for Span {
    fn from(value: pest::Span<'a>) -> Self {
        Self {
            start: value.start(),
            end: value.end(),
        }
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        (value.start, value.end - value.start).into()
    }
}

pub trait SpanIterExt {
    /// Merges a sequence of `Span` references into a single `Span`.
    ///
    /// The method computes a new `Span` that spans from the start of the first `Span`
    /// to the end of the last `Span` in the iterator. If the iterator contains only one
    /// `Span`, it returns that `Span` directly. If the iterator is empty, it returns `None`.
    fn merge_spans(self) -> Option<Span>;
}

impl<'a, T> SpanIterExt for T
where
    T: Iterator<Item = &'a Span>,
{
    fn merge_spans(mut self) -> Option<Span> {
        if let Some(first) = self.next() {
            if let Some(last) = self.last() {
                Some(Span {
                    start: first.start,
                    end: last.end,
                })
            } else {
                // Only one span, return it
                Some(*first)
            }
        } else {
            // No spans
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge_spans_multiple() {
        let spans = [
            Span { start: 1, end: 5 },
            Span { start: 6, end: 10 },
            Span { start: 11, end: 15 },
        ];

        let merged_span = spans.iter().merge_spans();
        assert_eq!(merged_span, Some(Span { start: 1, end: 15 }));
    }

    #[test]
    fn merge_spans_single() {
        let spans = [Span { start: 3, end: 7 }];

        let merged_span = spans.iter().merge_spans();
        assert_eq!(merged_span, Some(Span { start: 3, end: 7 }));
    }

    #[test]
    fn merge_spans_empty() {
        let spans = [];

        let merged_span = spans.iter().merge_spans();
        assert_eq!(merged_span, None);
    }
}
