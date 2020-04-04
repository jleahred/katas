use crate::ast::Node;
use idata::{cont::IVec, IString};

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Text(String),
    Function(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Template(pub(crate) Vec<Item>);

pub(crate) fn replace(ast: &Node) -> Result<String, String> {
    Ok(rec_replace(ast, Replaced("".to_string()))?.0)
}

#[derive(Debug)]
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
    fn update_by_name(
        node: &Node,
        map: im::HashMap<String, String>,
        repl: Replaced,
    ) -> im::HashMap<String, String> {
        match node {
            Node::Named((name, _)) => map.update(name.clone(), repl.0),
            Node::Rule((name, _nodes)) => map.update(name.clone(), repl.0),
            Node::Val(_) => map,
            Node::Transf2(_) => map,
            Node::EOF => map,
        }
    }

    let replaced_nodes = (Vec::<Replaced>::new(), im::HashMap::<String, String>::new());
    let replaced_nodes = nodes
        .iter()
        .fold(Ok(replaced_nodes), |acc, node| -> Result<_, String> {
            let replaced_node = || rec_replace(node, Replaced("".to_string()));
            match acc {
                Ok((by_index, by_name)) => Ok((
                    by_index.ipush(replaced_node()?),
                    update_by_name(node, by_name, replaced_node()?),
                )),
                Err(e) => Err(e),
            }
        });
    dbg!(replaced_nodes?);
    Ok(apply_transf2(template, repl))
}

fn apply_transf2(template: &Template, replaced: Replaced) -> Replaced {
    template
        .0
        .iter()
        .fold(replaced, |acc, repl_item| match repl_item {
            Item::Text(txt) => acc.iappend(txt),
            Item::Function(f) => acc.iappend(&format!("fn: {}", f)),
        })
}
