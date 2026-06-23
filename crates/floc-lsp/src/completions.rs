use std::collections::HashSet;

use floc_ast::{self as ast, Statement, TypeKind};
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
        CompletionItem {
            label: "si".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            label_details: Some(CompletionItemLabelDetails {
                detail: Some(" (expression) {statements}".to_string()),
                description: None,
            }),
            insert_text: Some("si (${1:expression}) {\n$0\n}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..CompletionItem::default()
        },
        // TODO: something smart for "sinon" & "sinon si"
        CompletionItem {
            label: "tantque".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            label_details: Some(CompletionItemLabelDetails {
                detail: Some(" (expression) {statements}".to_string()),
                description: None,
            }),
            insert_text: Some("tantque (${1:expression}) {\n$0\n}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..CompletionItem::default()
        },
    ]
}

pub fn variable_completion_items(program: &ast::Program, offset: usize) -> Vec<CompletionItem> {
    let mut vars: Vec<(String, TypeKind)> = Vec::new();

    if let Some(func) = program
        .function_decls
        .iter()
        .find(|func| func.span.contains(offset))
    {
        for arg in &func.arguments {
            vars.push((arg.name.ident.clone(), arg.r#type.kind.clone()));
        }
        collect_in_scope(&func.statements, offset, &mut vars);
    } else {
        collect_in_scope(&program.statements, offset, &mut vars);
    }

    // Keep the innermost (last pushed) declaration of each name so shadowing wins.
    let mut seen = HashSet::new();
    vars.into_iter()
        .rev()
        .filter(|(name, _)| seen.insert(name.clone()))
        .map(|(name, kind)| CompletionItem {
            label: name,
            kind: Some(CompletionItemKind::VARIABLE),
            label_details: Some(CompletionItemLabelDetails {
                detail: Some(format!(" {kind}")),
                description: None,
            }),
            ..CompletionItem::default()
        })
        .collect()
}

fn block_region(stmts: &[Statement]) -> Option<(usize, usize)> {
    let start = stmts.first()?.span().start;
    let end = stmts.last()?.span().end;
    Some((start, end))
}

fn collect_in_scope(stmts: &[Statement], offset: usize, vars: &mut Vec<(String, TypeKind)>) {
    for stmt in stmts {
        match stmt {
            Statement::Declaration(decl) if decl.span.end <= offset => {
                vars.push((decl.variable.ident.clone(), decl.r#type.kind.clone()));
            }
            Statement::While(whil) if whil.span.contains(offset) => {
                collect_in_scope(&whil.statements, offset, vars);
            }
            Statement::If(if_stmt) if if_stmt.span.contains(offset) => {
                let target = if_stmt
                    .statements_else
                    .as_ref()
                    .filter(|else_block| in_else_branch(if_stmt, else_block, offset))
                    .map_or(&if_stmt.statements, |else_block| else_block);
                collect_in_scope(target, offset, vars);
            }
            _ => {}
        }
    }
}

fn in_else_branch(if_stmt: &ast::If, else_block: &[Statement], offset: usize) -> bool {
    match block_region(else_block) {
        Some((else_start, _)) => offset >= else_start,
        None => block_region(&if_stmt.statements).is_some_and(|(_, then_end)| offset > then_end),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Variable names completed at the cursor `|` marked in `source`.
    ///
    /// Uses error-recovering parsing since completion fires while the source is
    /// mid-edit and often does not parse cleanly.
    fn completions_at_cursor(source: &str) -> Vec<String> {
        let offset = source
            .find('|')
            .expect("test source needs a `|` cursor marker");
        let source = source.replace('|', "");
        let (program, _) = floc_parser::parse_recover(&source);
        let program = program.expect("recovers a program");

        variable_completion_items(&program, offset)
            .into_iter()
            .map(|item| item.label)
            .collect()
    }

    #[test]
    fn suggests_top_level_variables_declared_before_the_cursor() {
        let names =
            completions_at_cursor("entier x = 1;\nentier y = 2;\necrire(|);\nentier z = 3;\n");
        assert!(names.contains(&"x".to_string()));
        assert!(names.contains(&"y".to_string()));
        // `z` is declared after the cursor, so it is not in scope yet.
        assert!(!names.contains(&"z".to_string()));
    }

    #[test]
    fn suggests_function_parameters_and_locals_but_not_top_level() {
        let names = completions_at_cursor(
            "entier f(entier a) {\n  entier b = 1;\n  retourner b|;\n}\nentier g = 9;\necrire(g);\n",
        );
        assert!(names.contains(&"a".to_string()));
        assert!(names.contains(&"b".to_string()));
        // Top-level variables are not visible inside a function body.
        assert!(!names.contains(&"g".to_string()));
    }

    #[test]
    fn inner_block_locals_are_visible_inside_the_block() {
        let names = completions_at_cursor(
            "entier x = 1;\ntantque (Vrai) {\n  entier y = 2;\n  ecrire(|);\n}\n",
        );
        assert!(names.contains(&"x".to_string()));
        assert!(names.contains(&"y".to_string()));
    }

    #[test]
    fn block_locals_do_not_leak_to_the_outer_scope() {
        let names = completions_at_cursor(
            "entier x = 1;\ntantque (Vrai) {\n  entier y = 2;\n}\necrire(|);\n",
        );
        assert!(names.contains(&"x".to_string()));
        assert!(!names.contains(&"y".to_string()));
    }

    #[test]
    fn then_block_locals_do_not_leak_into_else_block() {
        let names =
            completions_at_cursor("si (Vrai) {\n  entier t = 1;\n} sinon {\n  ecrire(|);\n}\n");
        assert!(!names.contains(&"t".to_string()));
    }

    #[test]
    fn shadowed_variable_is_listed_once() {
        let names = completions_at_cursor(
            "entier x = 1;\ntantque (Vrai) {\n  entier x = 2;\n  ecrire(|);\n}\n",
        );
        assert_eq!(names.iter().filter(|name| *name == "x").count(), 1);
    }
}
