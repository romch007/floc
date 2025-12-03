mod finder;

use std::collections::HashMap;
use std::sync::Arc;

use floc::analyzer::{self, Analyzer};
use floc::ast;
use floc::ast::visitor::Visitor;
use miette::NamedSource;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::finder::Finder;

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
            } else {
                offset += line.len() + 1; // +1 for newline
            }
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

    fn span_to_range_utf16(&self, span: &floc::utils::Span) -> Range {
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
        docs.get(uri).map(|doc| Arc::clone(&doc))
    }
}

impl Backend {
    async fn update_diagnostic(
        &self,
        analyze_result: core::result::Result<(), Box<analyzer::Error>>,
        document: Arc<Document>,
        uri: Url,
    ) {
        if let Err(e) = analyze_result {
            let message = e.to_string();
            let span = e.main_span();
            let range = span
                .map(|s| document.span_to_range_utf16(s))
                .unwrap_or_default();

            let diagnostic = Diagnostic {
                range,
                message,
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            };

            self.client
                .publish_diagnostics(uri, vec![diagnostic], None)
                .await;
        }
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
        let source = NamedSource::new("lsp.flo", params.text_document.text.clone());
        if let Ok(ast) = floc::parser::parse(source.clone()) {
            let mut analyzer = Analyzer::new(source.clone());
            let analyze_result = analyzer.analyze_program(&ast);

            let document = Arc::new(Document {
                program: ast,
                text: source,
            });
            self.documents
                .write()
                .await
                .insert(params.text_document.uri.clone(), document.clone());

            self.update_diagnostic(analyze_result, document, params.text_document.uri)
                .await;
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().next() {
            let source = NamedSource::new("lsp.flo", change.text.clone());
            if let Ok(ast) = floc::parser::parse(source.clone()) {
                let mut analyzer = Analyzer::new(source.clone());
                let analyze_result = analyzer.analyze_program(&ast);

                let document = Arc::new(Document {
                    program: ast,
                    text: source,
                });
                self.documents
                    .write()
                    .await
                    .insert(params.text_document.uri.clone(), document.clone());

                self.update_diagnostic(analyze_result, document, params.text_document.uri)
                    .await;
            }
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem {
                label: "entier".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..CompletionItem::default()
            },
            CompletionItem {
                label: "booleen".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..CompletionItem::default()
            },
            CompletionItem {
                label: "lire".to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                documentation: Some(Documentation::String("Read input from stdin".to_string())),
                label_details: Some(CompletionItemLabelDetails {
                    detail: Some("()".to_string()),
                    description: None,
                }),
                insert_text: Some("lire();".to_string()),
                insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                ..CompletionItem::default()
            },
            CompletionItem {
                label: "ecrire".to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                documentation: Some(Documentation::String("Write value to stdout".to_string())),
                label_details: Some(CompletionItemLabelDetails {
                    detail: Some("(val)".to_string()),
                    description: None,
                }),
                insert_text: Some("ecrire(${1:val});$0".to_string()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..CompletionItem::default()
            },
        ])))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        if let Some(doc) = self
            .get_document(&params.text_document_position_params.text_document.uri)
            .await
        {
            // TODO: cache this?
            let mut analyzer = Analyzer::new(doc.text.clone());
            let _ = analyzer.analyze_program(&doc.program);

            let offset = doc.pos_to_offset(params.text_document_position_params.position);

            let mut finder = Finder::new(offset);
            finder.visit_program(&doc.program);

            if let Some(ast::Expression::FunctionCall(fn_call)) = finder.found_expr {
                if let Some(function) = analyzer.functions().get(&fn_call.name.ident) {
                    return Ok(Some(get_function_doc(function)));
                }
            }
        }

        Ok(None)
    }
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
        markdown += param.kind.to_string().as_str();
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

    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
