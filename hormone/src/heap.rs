use std::collections::BTreeMap;

use crate::syntax::{Addr, Expr};


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HeapItem {
    pub expr: Expr,
    pub flag: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Heap {
    items: BTreeMap<Addr, HeapItem>,
    next: Addr,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            items: BTreeMap::new(),
            next: 1000,
        }
    }

    pub fn size(&self) -> usize {
        self.items.len()
    }

    pub fn get(&self, addr: Addr) -> Option<&Expr> {
        self.items.get(&addr).map(|i| &i.expr)
    }

    pub fn get_mut(&mut self, addr: Addr) -> Option<&mut Expr> {
        self.items.get_mut(&addr).map(|i| &mut i.expr)
    }

    pub fn set(&mut self, addr: Addr, expr: &Expr) {
        self.items.insert(addr, HeapItem { expr: expr.clone(), flag: false });
    }

    pub fn insert(&mut self, expr: &Expr) -> Addr {
        let addr = self.next;
        self.items.insert(addr, HeapItem { expr: expr.clone(), flag: false });
        self.next += 1;
        addr
    }

    pub fn remove(&mut self, addr: Addr) {
        self.items.remove(&addr);
    }

    pub fn has_flagged(&self, addr: Addr) -> bool {
        if let Some(i) = self.items.get(&addr) {
            i.flag
        } else {
            false
        }
    }

    pub fn flag(&mut self, addr: Addr) {
        if let Some(i) = self.items.get_mut(&addr) {
            i.flag = true;
        }
    }

    pub fn unflag_all(&mut self) {
        for i in self.items.values_mut() {
            i.flag = false;
        }
    }

    pub fn remove_all_unflagged(&mut self) {
        let unflagged_addrs = self.items.iter()
            .filter_map(|(k, v)| if !v.flag { Some(*k) } else { None })
            .collect::<Vec<_>>();
        for addr in unflagged_addrs {
            self.remove(addr);
        }
    }
}


