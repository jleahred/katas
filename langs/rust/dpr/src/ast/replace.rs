use crate::ast::Node;
use idata::IString;

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Text(String),
    Bypos(usize),
    ByName(String),
    Function(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Template(pub(crate) Vec<Item>);

impl Template {
    pub(crate) fn ipush(mut self, item: Item) -> Self {
        self.0.push(item);
        self
    }
}

#[derive(Debug)]
pub(crate) struct ReplacedNodes {
    by_pos: Vec<Replaced>,
    by_name: im::HashMap<String, Replaced>,
}

impl ReplacedNodes {
    fn new() -> Self {
        ReplacedNodes {
            by_pos: Vec::<Replaced>::new(),
            by_name: im::HashMap::<String, Replaced>::new(),
        }
    }

    fn process_node(mut self, node: &Node) -> Result<Self, String> {
        let node_replaced = replace(node)?;
        let node = dbg!(node);
        let node_replaced = dbg!(node_replaced);
        match node {
            Node::Named((name, _nodes)) => {
                self.by_pos.push(node_replaced.clone());
                self.by_name.insert(name.clone(), node_replaced);
            }
            Node::Rule((name, _nodes)) => {
                self.by_pos.push(node_replaced.clone());
                self.by_name.insert(name.clone(), node_replaced);
            }
            Node::Val(_) | Node::Transf2(_) | Node::EOF => (),
        }
        Ok(self)
    }
}

pub(crate) fn replace(ast: &Node) -> Result<Replaced, String> {
    Ok(rec_replace(ast, Replaced("".to_string()))?)
}

/// Replaced result
#[derive(Debug, Clone)]
pub struct Replaced(String);

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
    if nodes.len() > 0 {
        let replaced_nodes = nodes.iter().fold(Ok(ReplacedNodes::new()), |acc, node| {
            acc?.process_node(node)
        })?;
        Ok(apply_transf2(template, &replaced_nodes, repl))
    } else {
        Ok(repl)
    }
}

fn apply_transf2(
    template: &Template,
    replaced_nodes: &ReplacedNodes,
    replaced: Replaced,
) -> Replaced {
    template
        .0
        .iter()
        .fold(replaced, |acc, repl_item| match repl_item {
            Item::Text(txt) => acc.iappend(txt),
            Item::Bypos(p) => match replaced_nodes.by_pos.get(*p) {
                Some(rn) => acc.iappend(&format!("pos<{}/{}>", p, rn.0)),
                None => acc.iappend(&format!("pos<{}/missing>", p)),
            },
            Item::ByName(n) => match replaced_nodes.by_name.get(n) {
                Some(rn) => acc.iappend(&format!("{}", rn.0)),
                None => acc.iappend(&format!("name<{}/missing>", n)),
            },
            Item::Function(f) => acc.iappend(&format!("fn<{}>", f)),
        })
}
