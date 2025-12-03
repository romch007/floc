use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, Documentation, InsertTextFormat,
};

pub fn keyword_completion_items() -> Vec<CompletionItem> {
    vec![
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
            insert_text: Some("lire()".to_string()),
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
        CompletionItem {
            label: "retourner".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            label_details: Some(CompletionItemLabelDetails {
                detail: Some(" expression;".to_string()),
                description: None,
            }),
            insert_text: Some("retourner ${1:};$0".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..CompletionItem::default()
        },
    ]
}
