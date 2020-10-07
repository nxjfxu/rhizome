// Definition of all data structures

use std::collections::{BTreeMap, HashMap, BTreeSet};
use std::iter::FromIterator;

use crate::sanitize::sanitize;


pub type Addr = usize;

// AST

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum OpCode {
    // Identity
    This,

    // Mathematics
    Add, Min, Mul, Div,
    And, Or, Not,

    // Native text operations
    Text,
    Typeset,
    Link, Url,

    // Proliferation
    Cons, Car, Cdr,
    Pair,
    Empty,

    // Abstraction
    Lambda,
    ApplyTo,

    // Connexion
    Interweave,
    Include,

    // Determination
    If,

    // Failure
    Failure,
    Attempt,

    // Assignment
    Let,

    // Ontology
    Cluster,
    Extract,
    HasAttr,
    Update,
    Apply,

    // Metamorphosis
    Define,
    Set,

    // Communication
    Provide,
    Require,
    ProvideRequired,

    // Sequence
    Do,

    // Analysis
    Equals,
    IsClosure,
    // IsCell,
    // IsNil
    IsBoolean,
    IsString,
    IsException,
    IsInteger,
    IsOperator,
    IsObject,
    IsVoid,

    Native(&'static str),
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OpCode::*;

        match self {
            This => write!(f, "~."),

            Add => write!(f, "+"),
            Min => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),

            And => write!(f, "-*"),
            Or => write!(f, "-+"),
            Not => write!(f, "--"),

            Text => write!(f, ">~<"),
            Typeset => write!(f, "~~~"),

            Link => write!(f, "=>"),
            Url => write!(f, "/>"),

            Cons => write!(f, "><"),
            Car => write!(f, "<><"),
            Cdr => write!(f, "><>"),
            Pair => write!(f, "><?"),
            Empty => write!(f, ".?"),

            Lambda => write!(f, "\\"),
            ApplyTo => write!(f, ".>"),

            Interweave => write!(f, "<~<"),
            Include => write!(f, "<~~"),

            If => write!(f, "?"),

            Failure => write!(f, "!!"),
            Attempt => write!(f, ">!"),

            Let => write!(f, "<--"),

            Cluster => write!(f, ">+<"),
            Extract => write!(f, ">+"),
            HasAttr => write!(f, ">+?"),
            Update => write!(f, "+<"),
            Apply => write!(f, "->"),

            Define => write!(f, "<-"),
            Set => write!(f, "<!"),

            Provide => write!(f, "~>"),
            Require => write!(f, "<~"),
            ProvideRequired => write!(f, ">~>"),

            Do => write!(f, ">>"),

            Equals => write!(f, "="),
            IsClosure => write!(f, "^?"),
            IsBoolean => write!(f, "-?"),
            IsString => write!(f, "~?"),
            IsException => write!(f, "!?"),
            IsInteger => write!(f, "&?"),
            IsOperator => write!(f, "%?"),
            IsObject => write!(f, "+?"),
            IsVoid => write!(f, "??"),

            Native(n) => write!(f, "{}", n),
        }
    }
}

impl OpCode {
    pub fn from_str(op: &str) -> Option<Self> {
        use OpCode::*;

        match op {
            "~." => Some(This),

            "+" => Some(Add),
            "-" => Some(Min),
            "*" => Some(Mul),
            "/" => Some(Div),

            "-*" => Some(And),
            "-+" => Some(Or),
            "--" => Some(Not),

            ">~<" => Some(Text),
            "~~~" => Some(Typeset),

            "=>" => Some(Link),
            "/>" => Some(Url),

            /*
            "'" => Op(Quote),
             */

            "><" => Some(Cons),
            "<><" => Some(Car),
            "><>" => Some(Cdr),
            "><?" => Some(Pair),
            ".?" => Some(Empty),

            "\\" | "λ" | "lambda" => Some(Lambda),
            ".>" => Some(ApplyTo),

            "<~~" => Some(Include),
            "<~<" => Some(Interweave),

            "?" => Some(If),

            "!!" => Some(Failure),
            "!>" => Some(Attempt),

            "<--" => Some(Let),

            ">+<" => Some(Cluster),
            ">+" => Some(Extract),
            ">+?" => Some(HasAttr),
            "+<" => Some(Update),
            "->" => Some(Apply),

            "<-" => Some(Define),
            "<!" => Some(Set),

            "~>" => Some(Provide),
            "<~" => Some(Require),
            ">~>" => Some(ProvideRequired),

            ">>" => Some(Do),

            "=" => Some(Equals),
            "^?" => Some(IsClosure), // '^' in the Principia turned into lambda in LC
            "-?" => Some(IsBoolean),
            "~?" => Some(IsString),
            "!?" => Some(IsException),
            "&?" => Some(IsInteger),
            "%?" => Some(IsOperator),
            "+?" => Some(IsObject),
            "??" => Some(IsVoid),

            "~markdown" => Some(Native("markdown")),

            _ => None,
        }
    }
}



#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Context {
    tables: Vec<HashMap<String, Expr>>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            tables: vec![HashMap::new()],
        }
    }

    pub fn get(&self, x: &str) -> Option<&Expr> {
        for table in &self.tables {
            if let result@Some(_) = table.get(x) {
                return result;
            }
        }

        None
    }

    pub fn define(&mut self, x: &str, e: &Expr) {
        // May panic if there are no tables but this is not possible
        // if the context is constructed using new
        self.tables[0].insert(x.to_owned(), e.clone());
    }

    pub fn set(&mut self, x: &str, e: &Expr) {
        for table in self.tables.iter_mut() {
            if table.contains_key(x) {
                table.insert(x.to_owned(), e.clone());
                return;
            }
        }

        self.tables[0].insert(x.to_owned(), e.clone());
    }

    pub fn open(&mut self) {
        self.tables.insert(0, HashMap::new());
    }

    pub fn close(&mut self) {
        self.tables.remove(0);
    }

    pub fn append(&mut self, other: &Context) {
        for table in &other.tables {
            self.tables.push(table.clone());
        }
    }

    pub fn push_front(&mut self, other: &Context) {
        let mut temp = other.clone();
        temp.append(self);
        *self = temp;
    }

    pub fn condense(&self, used_variables: &BTreeSet<String>) -> Context {
        Context {
            tables: vec![
                used_variables.iter()
                    .filter_map(
                        |x| self.get(x).map(|e| (x.clone(), e.clone()))
                    ).collect()
            ],
        }
    }

    pub fn references(&self) -> BTreeSet<Addr> {
        let mut refs = BTreeSet::new();
        for table in self.tables.iter() {
            let mut expr_refs = BTreeSet::new();
            for e in table.values() {
                expr_refs = BTreeSet::from_iter(
                    expr_refs.union(&e.references())
                        .map(usize::to_owned)
                );
            }
            refs = BTreeSet::from_iter(
                refs.union(&expr_refs).map(usize::to_owned)
            );
        }
        refs
    }
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Arg {
    Collected(String),
    Individual(Vec<String>),
}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arg::Collected(x) => write!(f, "{}", x),
            Arg::Individual(xs) => write!(f, "({})", xs.join(" ")),
        }
    }
}

impl Arg {
    pub fn accepts(&self, args: &Vec<Expr>) -> bool {
        match self {
            Arg::Collected(_) => true,
            Arg::Individual(xs) => args.len() == xs.len(),
        }
    }

    pub fn bound(&self) -> Vec<&str> {
        match self {
            Arg::Collected(x) => vec![x],
            Arg::Individual(xs) => xs.iter().map(String::as_str).collect(),
        }
    }

    pub fn size(&self) -> String {
        match self {
            Arg::Collected(_) => String::from("[~+]"),
            Arg::Individual(xs) => xs.len().to_string(),
        }
    }
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Closure(Option<String>, Context, Arg, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),

    Cell(Box<Expr>, Box<Expr>),
    Nil,

    True, False,

    Var(String),
    Str(String),
    Exp(String),
    Int(isize),
    Op(OpCode),

    Ref(Addr),

    Object(Option<Box<Expr>>, BTreeMap<String, Expr>),

    Void,
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_inner(false))
    }
}

impl Expr {
    fn fmt_inner(&self, quoted: bool) -> String {
        use Expr::*;

        match self {
            Closure(Some(f), _, arg, _) => sanitize(&format!(
                "#<procedure:{}:{}>",
                f,
                arg.size()
            )),
            Closure(None, _, arg, _) => sanitize(&format!(
                "#<prodecure::{}>",
                arg.size()
            )),

            Cell(car_box, cdr_box) => if let Some(es) = self.to_list() {
                format!(
                    "{}({})",
                    if quoted { "" } else { "'" },
                    es.iter()
                        .map(|e| e.fmt_inner(true))
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            } else {
                format!(
                    "'({} . {})",
                    car_box.fmt_inner(quoted),
                    cdr_box.fmt_inner(quoted)
                )
            },

            Nil => String::from("."),

            True => "#+".to_string(),
            False => "#-".to_string(),

            Int(i) => i.to_string(),
            Str(s) => s.to_string(),
            Exp(s) => format!(
                "{}",
                html! {
                    span(class="error") { : "!!( " ; : s ; : " )" }
                }),
            Op(op) => sanitize(&format!("#<op:{}>", op)),
            Var(name) => format!("${}", sanitize(name)),
            App(func_box, args_box) => format!(
                "({} {})",
                func_box.fmt_inner(quoted),
                args_box.iter()
                    .map(|eb| eb.fmt_inner(quoted))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),

            Ref(addr) => format!("&<{}>", addr),

            Object(possible_super, attributes) => {
                let pad_width = attributes.iter()
                    .map(|(k, _)| k.len())
                    .max().unwrap_or(0);
                let empty = attributes.is_empty();

                format!(
                    "{}{{{}{}}}",
                    possible_super.as_ref()
                        .map(|eb| eb.fmt_inner(quoted))
                        .unwrap_or(String::from("")),
                    attributes.iter()
                        .map(|(k, v)| format!("\n  :{k:>w$} {v}",
                                              k = k,
                                              w = pad_width,
                                              v = v
                        ))
                        .collect::<Vec<_>>()
                        .join(""),
                    if empty { " " } else { "\n" }
                )
            },

            Void => String::from(""),
        }
    }

    pub fn is_normal(&self) -> bool {
        use Expr::*;

        match self {
            Op(OpCode::This) => false,

            Closure(_, _, _, _) |
            Cell(_, _) | Nil |
            True | False |
            Int(_) |
            Str(_) |
            Exp(_) |
            Op(_) |
            Ref(_) |
            Object(_, _) |
            Void
                => true,

            App(_, _) | Var(_) => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Expr::Nil => true,
            Expr::Cell(_, cdr_box) => cdr_box.is_list(),
            _ => false,
        }
    }

    pub fn to_list(&self) -> Option<Vec<Expr>> {
        match self {
            Expr::Nil => Some(vec![]),
            Expr::Cell(car_box, cdr_box) => {
                let mut cdr_list = cdr_box.to_list()?;
                cdr_list.insert(0, *car_box.clone());
                Some(cdr_list)
            },
            _ => None,
        }
    }

    pub fn from_slice(es: &[Expr]) -> Self {
        es.iter().rev().fold(
            Expr::Nil,
            |l, e| Expr::Cell(Box::new(e.clone()), Box::new(l))
        )
    }

    pub fn from_bool(b: bool) -> Self {
        if b {
            Expr::True
        } else {
            Expr::False
        }
    }

    pub fn free_variables(&self) -> BTreeSet<String> {
        use Expr::*;

        match self {
            Closure(_, _, arg, body) => BTreeSet::from_iter(
                body.free_variables()
                    .difference(&BTreeSet::from_iter(
                        arg.bound().into_iter().map(str::to_string)
                    )).map(String::to_owned)
            ),

            App(fun_box, arg_boxes) => BTreeSet::from_iter(
                arg_boxes.iter()
                    .map(|eb| eb.free_variables())
                    .fold(
                        fun_box.free_variables(),
                        |a, fvs| BTreeSet::from_iter(
                            a.union(&fvs).map(String::to_owned)
                        )
                    )
            ),

            Cell(car_box, cdr_box) => BTreeSet::from_iter(
                car_box.free_variables()
                    .union(&cdr_box.free_variables())
                    .map(String::to_owned)
            ),

            Var(x) => {
                let mut set = BTreeSet::new();
                set.insert(x.to_owned());
                set
            },

            _ => BTreeSet::new(),

        }
    }

    pub fn references(&self) -> BTreeSet<Addr> {
        use Expr::*;

        match self {
            Object(s, bindings) => BTreeSet::from_iter(
                bindings.values()
                    .map(|e| e.references())
                    .fold(s.as_ref().map(|eb| eb.references())
                          .unwrap_or(BTreeSet::new()),
                          |a, refs| BTreeSet::from_iter(
                              a.union(&refs).map(usize::to_owned)
                          )
                    )
            ),

            Closure(_, ctx, _, body) => BTreeSet::from_iter(
                body.references().union(&ctx.references())
                    .map(usize::to_owned)
            ),

            App(fun_box, arg_boxes) => BTreeSet::from_iter(
                arg_boxes.iter()
                    .map(|eb| eb.references())
                    .fold(
                        fun_box.references(),
                        |a, fvs| BTreeSet::from_iter(
                            a.union(&fvs).map(usize::to_owned)
                        )
                    )
            ),

            Cell(car_box, cdr_box) => BTreeSet::from_iter(
                car_box.references()
                    .union(&cdr_box.references())
                    .map(usize::to_owned)
            ),

            Ref(addr) => {
                let mut set = BTreeSet::new();
                set.insert(addr.to_owned());
                set
            },

            _ => BTreeSet::new(),
        }
    }
}


// For parser

pub type Srcloc = (usize, usize);

#[derive(Debug, PartialEq, Eq)]
pub enum Ast {
    /*
    Quote(Srcloc, Box<Ast>),
    */
    RawText(Srcloc, String),
    Text(Srcloc, String),
    Atom(Srcloc, String),
    List(Srcloc, Vec<Ast>),
    Fabric(Srcloc, Vec<Ast>),
}

