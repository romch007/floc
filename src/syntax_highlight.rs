use miette::highlighters::{Highlighter, HighlighterState};
use owo_colors::{Style, Styled};
use regex::Regex;

pub struct FloHighlighter;

impl Highlighter for FloHighlighter {
    fn start_highlighter_state<'h>(
        &'h self,
        _source: &dyn miette::SpanContents<'_>,
    ) -> Box<dyn HighlighterState + 'h> {
        Box::new(FloHighlighterState::new())
    }
}

#[derive(Debug)]
struct FloHighlighterState {
    re: Regex,
}

impl FloHighlighterState {
    pub fn new() -> Self {
        Self {
            re: Regex::new(r"([a-zA-Z_][a-zA-Z_0-9]*|\d+|\s+|[;(){}=,])").unwrap(),
        }
    }
}

impl HighlighterState for FloHighlighterState {
    fn highlight_line<'s>(&mut self, line: &'s str) -> Vec<Styled<&'s str>> {
        let mut styled_parts = Vec::new();

        for mat in self.re.find_iter(line) {
            let word = mat.as_str();

            let style =
                if ["si", "sinon", "tantque", "retourner", "non", "et", "ou"].contains(&word) {
                    Style::new().red()
                } else if ["entier", "booleen"].contains(&word) {
                    Style::new().yellow()
                } else if ["lire", "ecrire"].contains(&word) {
                    Style::new().blue()
                } else if ["Vrai", "Faux"].contains(&word) || word.parse::<u64>().is_ok() {
                    Style::new().purple()
                } else {
                    Style::default()
                };

            styled_parts.push(style.style(word));
        }

        styled_parts
    }
}
