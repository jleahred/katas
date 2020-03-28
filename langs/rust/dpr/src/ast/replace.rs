use crate::ast::Node;
use idata::IString;

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Text(String),
    Function(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Template {
    pub items: Vec<Item>,
}

pub(crate) fn replace(ast: &Node) -> Result<String, String> {
    Ok(rec_replace(ast, Replaced("".to_string()))?.0)
}

struct Replaced(String);

impl Replaced {
    fn iappend(self, txt: &str) -> Self {
        Self(self.0.iappend(txt))
    }
}

fn rec_replace(ast: &Node, repl: Replaced) -> Result<Replaced, String> {
    match ast {
        Node::EOF => Ok(repl),
        Node::Val(s) => Ok(repl.iappend(s)),
        Node::Named((_, nodes)) => rec_replace_nodes(nodes, repl),
        Node::Transf2(crate::ast::Transf2 { template, nodes }) => {
            rec_transf2_nodes(nodes, template, repl)
        }
        Node::Rule((_, nodes)) => rec_replace_nodes(nodes, repl),
    }
}

fn rec_replace_nodes(nodes: &[Node], repl: Replaced) -> Result<Replaced, String> {
    nodes.iter().fold(Ok(repl), |acc, node| match acc {
        Ok(repl) => rec_replace(node, repl),
        Err(e) => Err(e),
    })
}

fn rec_transf2_nodes(
    nodes: &[Node],
    template: &Template,
    repl: Replaced,
) -> Result<Replaced, String> {
    let replaced = nodes //  todo
        .iter()
        .fold(Ok(Replaced("".to_string())), |acc, node| match acc {
            Ok(repl) => rec_replace(node, repl),
            Err(e) => Err(e),
        })?;
    Ok(apply_transf2(template, repl))
}

fn apply_transf2(template: &Template, replaced: Replaced) -> Replaced {
    template
        .items
        .iter()
        .fold(replaced, |acc, repl_item| match repl_item {
            Item::Text(txt) => acc.iappend(txt),
            Item::Function(f) => acc.iappend(f),
        })
}
