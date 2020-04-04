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
}
