//----------------------------
#[derive(Debug)]
enum IList {
    End,
    Cons(i32, Box<IList>),
}

impl IList {
    fn push_front(self, i: i32) -> Self {
        IList::Cons(i, Box::new(self))
    }

    fn sum(&self) -> i32 {
        match self {
            IList::End => 0,
            IList::Cons(i, il) => i + il.sum(),
        }
    }

    fn append(self, other: &IList) -> Self {
        match other {
            IList::End => IList::End,
            IList::Cons(i, il) => self.push_front(*i).append(il.as_ref()),
        }
    }

    fn map<F>(self, f: F) -> IList
    where
        F: Fn(i32) -> i32,
    {
        match self {
            IList::End => self,
            IList::Cons(i, il) => IList::Cons(f(i), Box::new(il.map(f))),
        }
    }
}

//----------------------------
#[derive(Debug)]
enum ITree {
    Leaf(i32),
    Branch((Box<ITree>, Box<ITree>)),
}

impl ITree {
    fn sum(&self) -> i32 {
        match self {
            ITree::Leaf(n) => *n,
            ITree::Branch((l, r)) => l.sum() + r.sum(),
        }
    }
}

trait Fold {
    fn fold<F>(&self, acc: i32, f: &F) -> i32
    where
        F: Fn(i32, i32) -> i32;
}

impl Fold for ITree {
    fn fold<F>(&self, acc: i32, f: &F) -> i32
    where
        F: Fn(i32, i32) -> i32,
    {
        match self {
            ITree::Leaf(i) => f(acc, *i),
            ITree::Branch((l, r)) => {
                let acc = l.fold(acc, f);
                r.fold(acc, f)
            }
        }
    }
}

trait FoldG {
    fn foldg<ACC, F>(&self, acc: ACC, f: &F) -> ACC
    where
        F: Fn(ACC, i32) -> ACC;
}

impl FoldG for ITree {
    fn foldg<ACC, F>(&self, acc: ACC, f: &F) -> ACC
    where
        F: Fn(ACC, i32) -> ACC,
    {
        match self {
            ITree::Leaf(i) => f(acc, *i),
            ITree::Branch((l, r)) => {
                let acc = l.foldg(acc, f);
                r.foldg(acc, f)
            }
        }
    }
}

//----------------------------
fn main() {
    let il = IList::End;

    println!("list... {:#?}", il);

    let il = IList::Cons(32, Box::new(IList::End));
    println!("list... {:#?}", il);

    let il = il.push_front(33);
    println!("list... {:#?}", il);

    let il = il.push_front(3).push_front(2).push_front(1).push_front(0);
    println!("list... {:#?}", il);

    println!("sum... {:#?}", il.sum());

    println!("map... {:#?}", il.map(|i| i + 1));

    let il1 = IList::End.push_front(1).push_front(2).push_front(3);
    let il2 = IList::End.push_front(21).push_front(22).push_front(23);
    println!("append... {:#?}", il1.append(&il2));

    let tree = ITree::Branch((
        Box::new(ITree::Branch((
            Box::new(ITree::Leaf(1)),
            Box::new(ITree::Leaf(2)),
        ))),
        Box::new(ITree::Leaf(3)),
    ));
    println!("tree... {:#?}", tree);
    println!("tree... {:#?}", tree.sum());

    println!("tree... {:#?}", tree.fold(0, &|acc, i| acc + i));
    println!("tree... {:#?}", tree.foldg(0, &|acc, i| acc + i));
}
