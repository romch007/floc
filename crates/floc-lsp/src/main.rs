mod completions;
mod finder;
mod references;

use std::collections::HashMap;
use std::sync::Arc;

use floc_analyzer::{self as analyzer, Analyzer};
use floc_ast::{self as ast, visitor::Visitor};
use floc_span::Span;
use miette::NamedSource;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionOptions,
    CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentHighlight,
    DocumentHighlightKind, DocumentHighlightParams, Hover, HoverContents, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams,
    InsertTextFormat, MarkupContent, MarkupKind, MessageType, OneOf, Position, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::completions::keyword_completion_items;
use crate::finder::Finder;
use crate::references::{RefKind, References};

#[derive(Debug)]
struct Document {
    pub text: miette::NamedSource<String>,
    pub program: ast::Program,
}

impl Document {
    fn pos_to_offset(&self, position: Position) -> usize {
        let mut offset = 0;
        for (line_idx, line) in self.text.inner().lines().enumerate() {
            if line_idx == position.line as usize {
                offset += position.character as usize;
                break;
            }
            offset += line.len() + 1; // +1 for newline
        }
        offset
    }

    fn offset_to_pos_utf16(&self, offset: usize) -> Position {
        let mut line = 0u32;
        let mut character = 0u32;
        let mut count = 0usize;

        for ch in self.text.inner().as_str().chars() {
            if count >= offset {
                break;
            }

            if ch == '\n' {
                line += 1;
                character = 0;
            } else {
                character += ch.len_utf16() as u32;
            }

            count += ch.len_utf8();
        }

        Position { line, character }
    }

    fn span_to_range_utf16(&self, span: &Span) -> Range {
        Range {
            start: self.offset_to_pos_utf16(span.start),
            end: self.offset_to_pos_utf16(span.end),
        }
    }
}

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: RwLock<HashMap<Url, Arc<Document>>>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: Default::default(),
        }
    }

    async fn get_document(&self, uri: &Url) -> Option<Arc<Document>> {
        let docs = self.documents.read().await;
        docs.get(uri).map(Arc::clone)
    }
}

impl Backend {
    async fn update_diagnostic(&self, diagnostics: Vec<Diagnostic>, uri: Url) {
        // An empty list clears previously reported diagnostics.
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions::default()),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_highlight_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let (document, diagnostics) = analyze_source(params.text_document.text);

        self.documents.write().await.insert(uri.clone(), document);
        self.update_diagnostic(diagnostics, uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().next() {
            let uri = params.text_document.uri;
            let (document, diagnostics) = analyze_source(change.text);

            self.documents.write().await.insert(uri.clone(), document);
            self.update_diagnostic(diagnostics, uri).await;
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let position = params.text_document_position_params;

        let Some(doc) = self.get_document(&position.text_document.uri).await else {
            return Ok(None);
        };

        let offset = doc.pos_to_offset(position.position);
        let references = References::collect(&doc.program);

        let highlights = references
            .at_offset(offset)
            .into_iter()
            .map(|reference| DocumentHighlight {
                range: doc.span_to_range_utf16(&reference.span),
                kind: Some(match reference.kind {
                    RefKind::Read => DocumentHighlightKind::READ,
                    RefKind::Write => DocumentHighlightKind::WRITE,
                }),
            })
            .collect::<Vec<_>>();

        if highlights.is_empty() {
            Ok(None)
        } else {
            Ok(Some(highlights))
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut completions: Vec<CompletionItem> = Vec::new();

        // Add keywords + functions from the standard lib
        completions.extend(keyword_completion_items());

        let uri = params.text_document_position.text_document.uri;
        if let Some(doc) = self.get_document(&uri).await {
            let mut analyzer = Analyzer::new(doc.text.clone());
            analyzer.analyze_program(&doc.program);

            // Add all functions to completion
            for (name, function) in analyzer.functions() {
                let mut details = String::from("(");
                let mut snippet = format!("{name}(");

                for (i, param) in function.arguments.iter().enumerate() {
                    details += &format!("{} {}", param.1.kind, param.0);
                    if i < function.arguments.len() - 1 {
                        details += ", ";
                    }

                    snippet += &format!("${{{}}}", param.0);
                    if i < function.arguments.len() - 1 {
                        snippet += ", ";
                    }
                }

                details += ")";
                // TODO: something smart to check whether or not we should add a semicolon
                snippet += ")$0";

                let item = CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    label_details: Some(CompletionItemLabelDetails {
                        detail: Some(details),
                        description: None,
                    }),
                    insert_text: Some(snippet),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                };

                completions.push(item);
            }
        }

        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        if let Some(doc) = self
            .get_document(&params.text_document_position_params.text_document.uri)
            .await
        {
            // TODO: cache this?
            // Il veut jouer à cache-cache ?
            let mut analyzer = Analyzer::new(doc.text.clone());
            analyzer.analyze_program(&doc.program);

            let offset = doc.pos_to_offset(params.text_document_position_params.position);

            let mut finder = Finder::new(offset);
            finder.visit_program(&doc.program);

            if let Some(ast::Expression::FunctionCall(fn_call)) = finder.found_expr
                && let Some(function) = analyzer.functions().get(&fn_call.name.ident)
            {
                return Ok(Some(get_function_doc(function)));
            }
        }

        Ok(None)
    }
}

/// Parse and analyze `text`, producing the document and the diagnostics to
/// publish. Parse errors are reported on their own; semantic analysis only runs
/// once the source parses cleanly, to avoid cascading noise from poison nodes.
fn analyze_source(text: String) -> (Arc<Document>, Vec<Diagnostic>) {
    let source = NamedSource::new("lsp.flo", text);
    let (program, parse_diagnostics) = floc_parser::parse_recover(source.inner().as_str());

    let document = Arc::new(Document {
        program: program.unwrap_or_else(|| ast::Program {
            function_decls: Vec::new(),
            statements: Vec::new(),
        }),
        text: source.clone(),
    });

    let diagnostics = if parse_diagnostics.is_empty() {
        // The non-`Send` analyzer stays in this sync function, away from awaits.
        let mut analyzer = Analyzer::new(source);
        analyzer.analyze_program(&document.program);
        collect_diagnostics(&analyzer, &document)
    } else {
        parse_diagnostics
            .into_iter()
            .map(|diagnostic| Diagnostic {
                range: document.span_to_range_utf16(&diagnostic.span),
                message: diagnostic.message,
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            })
            .collect()
    };

    (document, diagnostics)
}

fn collect_diagnostics(analyzer: &Analyzer, document: &Document) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    for error in analyzer.errors() {
        let range = error
            .main_span()
            .map(|s| document.span_to_range_utf16(s))
            .unwrap_or_default();

        diagnostics.push(Diagnostic {
            range,
            message: error.to_string(),
            severity: Some(DiagnosticSeverity::ERROR),
            ..Default::default()
        });
    }

    for warning in analyzer.warnings() {
        let range = warning
            .main_span()
            .map(|s| document.span_to_range_utf16(s))
            .unwrap_or_default();

        diagnostics.push(Diagnostic {
            range,
            message: warning.to_string(),
            severity: Some(DiagnosticSeverity::WARNING),
            ..Default::default()
        });
    }

    diagnostics
}

fn get_function_doc(function: &analyzer::Function) -> Hover {
    let mut markdown = String::new();

    markdown += "function ";
    markdown += "**";
    markdown += function.name.as_str();
    markdown += "**";

    markdown += "\n\n---\n\n";

    markdown += "-> **";
    markdown += function.return_type.kind.to_string().as_str();
    markdown += "**\n";

    markdown += "\n\n---\n\n";

    markdown += "parameters:";
    markdown += "\n";

    for param in &function.arguments {
        markdown += " - **";
        markdown += param.1.kind.to_string().as_str();
        markdown += " ";
        markdown += param.0.as_str();
        markdown += "**\n";
    }

    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: markdown,
        }),
        range: None,
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
