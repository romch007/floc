mod completions;
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

use crate::completions::keyword_completion_items;
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
                    text: source,
                }),
            );
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().next() {
            let source = NamedSource::new("lsp.flo", change.text.clone());
            if let Ok(ast) = floc::parser::parse(source.clone()) {
                self.documents.write().await.insert(
                    params.text_document.uri,
                    Arc::new(Document {
                        program: ast,
                        text: source,
                    }),
                );
            }
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(keyword_completion_items())))
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
