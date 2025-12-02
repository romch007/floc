mod finder;

use std::collections::HashMap;
use std::fmt::Write;
use std::sync::Arc;

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
    pub text: String,
    pub program: ast::Program,
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
            self.documents.write().await.insert(
                params.text_document.uri.clone(),
                Arc::new(Document {
                    program: ast,
                    text: params.text_document.text.clone(),
                }),
            );
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().next() {
            let source = NamedSource::new("lsp.flo", change.text.clone());
            if let Ok(ast) = floc::parser::parse(source) {
                self.documents.write().await.insert(
                    params.text_document.uri,
                    Arc::new(Document {
                        program: ast,
                        text: change.text.clone(),
                    }),
                );
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
            let offset =
                position_to_offset(&doc.text, params.text_document_position_params.position);
            let mut finder = Finder::new(offset);
            finder.visit_program(&doc.program);

            if let Some(expr) = finder.found_expr {
                let mut s = String::new();
                write!(&mut s, "{expr:?}").unwrap();
                self.client.log_message(MessageType::LOG, &s).await;
            }
        }

        Ok(None)
    }
}

fn position_to_offset(text: &str, position: Position) -> usize {
    let mut offset = 0;
    for (line_idx, line) in text.lines().enumerate() {
        if line_idx == position.line as usize {
            offset += position.character as usize;
            break;
        } else {
            offset += line.len() + 1; // +1 for newline
        }
    }
    offset
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
