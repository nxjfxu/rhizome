use horrorshow::prelude::*;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::iter::FromIterator;


use crate::heap::*;
use crate::parse::*;
use crate::syntax::*;

use crate::sanitize::sanitize;

use OpCode::*;
use Expr::*;


static PRELUDE_ID: &'static str = ".^";


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EvalError {
    TypeError(&'static str),
    Undefined(String),
    ArgumentError(String),
    MathError(&'static str),
    NotFound(String),
    Unsupported(String),

    Timeout,

    OtherError(String),

    InternalError(&'static str),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError(t) => write!(f, "Expecting a {}.", t),
            Undefined(x) => write!(f, "'{}' is undefined.", x),
            ArgumentError(s) => write!(f, "Argument error: {}", s),
            MathError(s) => write!(f, "Math error: {}", s),
            NotFound(x) => write!(f, "'{}' is not found in object.", x),
            Unsupported(s) => write!(f, "'{}' is unsupported.", s),

            Timeout => write!(f, "Evaluation took too long"),

            OtherError(s) => write!(f, "{}", s),

            InternalError(s) => write!(f, "The evaluator failed unexpectedly: {}", s),
        }
    }
}


impl std::error::Error for EvalError { }

use EvalError::*;


pub type LookupFn<'l>  = dyn 'l + Fn(&str) -> Option<String>;
pub type StopFn        = dyn Fn()     -> bool;
pub type EvalResult<T> = Result<T, EvalError>;


fn to_int(expr: Expr) -> EvalResult<isize> {
    match expr {
        Int(i) => Ok(i),
        _ => Err(TypeError("integer")),
    }
}

fn to_str(expr: Expr) -> EvalResult<String> {
    match expr {
        Str(s) => Ok(s),
        _ => Err(TypeError("string")),
    }
}

fn parse_bindings(bindings: &[Expr]) -> EvalResult<(Vec<String>, Vec<Expr>)> {
    let mut xs = Vec::new();
    let mut es = Vec::new();
    for e in bindings.iter() {
        if let App(xb, ebs) = e {
            if let Var(x) = &**xb {
                xs.push(x.clone());
                es.push(
                    ebs.get(0).ok_or(ArgumentError(String::from(
                        "Binding pairs must have exactly two elements."
                    )))?.clone()
                );
            } else {
                return Err(ArgumentError(String::from(format!(
                    "The first element in a binding pair must be a variable, got {} instead.",
                    xb
                ))));
            }
        } else {
            return Err(ArgumentError(String::from(format!(
                "Expecting a binding pair, got {} instead.",
                e
            ))));
        }
    }
    Ok((xs, es))
}


pub struct EvaluatorConfig<'l, 's> {
    lookup_fn: &'l LookupFn<'l>,
    stop_fn: Option<&'s StopFn>,

    page_extension: Option<String>,
}

impl<'l, 's> Default for EvaluatorConfig<'l, 's> {
    fn default() -> Self {
        EvaluatorConfig {
            lookup_fn: &|_| None,
            stop_fn: None,

            page_extension: None,
        }
    }
}

impl<'l, 's> EvaluatorConfig<'l, 's> {
    pub fn new() -> Self {
        EvaluatorConfig::default()
    }

    pub fn with_lookup(self, lookup: &'l LookupFn) -> Self {
        EvaluatorConfig { lookup_fn: lookup, ..self }
    }

    pub fn with_stop(self, stop: &'s StopFn) -> Self {
        EvaluatorConfig { stop_fn: Some(stop), ..self }
    }

    pub fn with_page_extension(self, ext: &str) -> Self {
        if ext.len() > 0 {
            EvaluatorConfig {
                page_extension: Some(format!(".{}", ext)),
                ..self
            }
        } else {
            self
        }
    }

    pub fn for_item(self, iid: &str) -> Evaluator<'l, 's> {
        let page_extension = self.page_extension.unwrap_or(String::from(""));
        let mut context = Context::new();
        context.define("~page-extension", &Str(page_extension.clone()));

        Evaluator {
            lookup_fn: self.lookup_fn,
            stop_fn: self.stop_fn,

            page_extension,

            current_item_stack: vec![iid.to_string()],

            heap: Heap::new(),
            context,
            context_stack: vec![],

            required: BTreeMap::new(),
            available_required_stack: vec![BTreeSet::new()],

            next_fresh: 0,

            cycles: 0,
        }
    }
}


pub struct Evaluation {
    pub expr: Expr,
    pub cycles: usize,
    pub context: Context,
    pub heap: Heap,
}


pub struct Evaluator<'l, 's> {
    lookup_fn: &'l LookupFn<'l>,
    stop_fn: Option<&'s StopFn>,

    page_extension: String,

    current_item_stack: Vec<String>,

    heap: Heap,
    context: Context,
    context_stack: Vec<Context>,

    required: BTreeMap<String, HashMap<String, Expr>>,
    available_required_stack: Vec<BTreeSet<String>>,

    next_fresh: isize,

    cycles: usize,
}

impl<'l, 's> Evaluator<'l, 's> {
    fn lookup(&self, iid: &str) -> Option<String> {
        (self.lookup_fn)(iid)
    }

    fn stop(&self) -> bool {
        if let Some(f) = self.stop_fn {
            f()
        } else {
            false
        }
    }

    fn fresh(&mut self) -> Expr {
        self.next_fresh += 1;
        Var(format!("fresh:{}", self.next_fresh))
    }


    fn is_required_available(&self, iid: &str) -> bool {
        iid == PRELUDE_ID || self.available_required_stack.last()
            .map(|set| set.contains(iid))
            .unwrap_or(false)
    }

    fn get_variable_value(&self, x: &str) -> Option<&Expr> {
        self.context.get(x)
            .or({
                let mut found = None;
                for (iid, table) in self.required.iter() {
                    if self.is_required_available(iid) {
                        found = table.get(x);
                    }
                    if found.is_some() {
                        break;
                    }
                }
                found
            })
    }

    fn current_item(&self) -> &String {
        self.current_item_stack.last().unwrap()
    }

    fn apply_op(&mut self, op: OpCode, mut args: Vec<Expr>) -> EvalResult<Expr> {
        match op {
            Text => {
                let mut results = Vec::with_capacity(args.len());
                for arg in args {
                    results.push(match arg {
                        Ref(addr) => match self.heap.get(addr) {
                            Some(e) => e.to_string(),
                            None => return Err(Unsupported(addr.to_string())),
                        },
                        e => e.to_string(),
                    });
                }

                Ok(Str(results.join("")))
            },

            Link => {
                use std::path::{Path, PathBuf};

                let dest = to_str(args.get(0).ok_or(ArgumentError(String::from(
                    "Expecting an ID as the argument."
                )))?.clone())?;

                let (path, fragment) = {
                    let mut dest_parts = dest.splitn(2, "#");
                    (
                        dest_parts.next().unwrap_or("."),
                        dest_parts.next()
                    )
                };

                let path_to_current = Path::new(self.current_item());
                let path_to_dest = Path::new(&path);
                let mut final_path = path_to_current.parent()
                    .unwrap()
                    .components()
                    .fold(PathBuf::from("."), |pb, _| pb.join(".."));
                final_path.push(path_to_dest);

                Ok(Str(format!(
                    "{}",
                    html!{ a(class="anchor",
                             href = Raw(format!(
                                 "{}{}{}{}",
                                 &final_path.display(),
                                 &self.page_extension,
                                 fragment.map(|_| "#").unwrap_or(""),
                                 fragment.unwrap_or(""),
                             )))
                           : Raw(format!("[{}]", &dest))
                    })))
            },

            Url => {
                let target = to_str(
                    args.get(0).ok_or(ArgumentError(String::from(
                        "Expecting a URL as the argument."
                    )))?.clone()
                )?;
                Ok(Str(format!("{}",
                               html!{ a(class="anchor",
                                        href = Raw(&target))
                                      : Raw(format!("<[{}]>", &target))
                               })))
            },

            Add => args.into_iter()
                .map(to_int)
                .fold(Ok(0), |ar, er| ar.and_then(|a : isize| if let Ok(e) = er {
                    a.checked_add(e).ok_or(MathError("Addition overflowed."))
                } else {
                    er
                }))
                .map(Int),

            Min if args.len() == 0 => Ok(Int(0)),

            Min => args[1..].iter()
                .map(Expr::clone)
                .map(to_int)
                .fold(
                    Ok(to_int(args[0].clone())?),
                    |ar, er| ar.and_then(|a : isize| if let Ok(e) = er {
                        a.checked_sub(e).ok_or(MathError("Subtraction underflowed."))
                    } else {
                        er
                    }))
                .map(Int),

            Mul => args.into_iter()
                .map(to_int)
                .fold(Ok(1), |ar, er| ar.and_then(|a : isize| if let Ok(e) = er {
                    a.checked_mul(e).ok_or(MathError("Multiplication overflowed."))
                } else {
                    er
                }))
                .map(Int),

            Div => args.get(1..)
                .ok_or(ArgumentError(String::from(
                    "Expecting a number to be divided."
                )))
                .and_then(
                    |rest| rest.iter()
                        .map(Expr::clone)
                        .map(to_int)
                        .fold(Ok(to_int(args[0].clone())?), |ar, er|
                              ar.and_then(|a| if let Ok(e) = er {
                                  a.checked_div(e).ok_or(MathError("Division by zero."))
                              } else {
                                  er
                              }))
                        .map(Int)),

            Le => if args.len() == 2 {
                let a1 = to_int(args.remove(0))?;
                let a2 = to_int(args.remove(0))?;
                Ok(Expr::from_bool(a1 < a2))
            } else {
                Err(ArgumentError(String::from(
                    "Mathematical comparison takes two arguments."
                )))
            },
            Leq => if args.len() == 2 {
                let a1 = to_int(args.remove(0))?;
                let a2 = to_int(args.remove(0))?;
                Ok(Expr::from_bool(a1 <= a2))
            } else {
                Err(ArgumentError(String::from(
                    "Mathematical comparison takes two arguments."
                )))
            },
            Geq => if args.len() == 2 {
                let a1 = to_int(args.remove(0))?;
                let a2 = to_int(args.remove(0))?;
                Ok(Expr::from_bool(a1 >= a2))
            } else {
                Err(ArgumentError(String::from(
                    "Mathematical comparison takes two arguments."
                )))
            },
            Gr => if args.len() == 2 {
                let a1 = to_int(args.remove(0))?;
                let a2 = to_int(args.remove(0))?;
                Ok(Expr::from_bool(a1 > a2))
            } else {
                Err(ArgumentError(String::from(
                    "Mathematical comparison takes two arguments."
                )))
            },

            Cons => if args.len() == 2 {
                Ok(Cell(
                    Box::new(args.get(0).unwrap().clone()),
                    Box::new(args.get(1).unwrap().clone())
                ))
            } else {
                Err(ArgumentError(String::from(
                    "A 'cons' operation takes two arguments."
                )))
            },
            Car => if let Cell(car_box, _) = args.get(0).ok_or(ArgumentError(
                String::from("Expecting a 'cons' cell as the argument.")
            ))? {
                Ok(*car_box.clone())
            } else {
                Err(TypeError("pair"))
            },
            Cdr => if let Cell(_, cdr_box) = args.get(0).ok_or(ArgumentError(
                String::from("Expecting a 'cons' cell as the argument.")
            ))? {
                Ok(*cdr_box.clone())
            } else {
                Err(TypeError("pair"))
            },
            Pair => Ok(if let Cell(_, _) = args.get(0).ok_or(ArgumentError(
                String::from("Expecting an argument.")
            ))? {
                True
            } else {
                False
            }),
            Empty => Ok(if let Nil = args.get(0).ok_or(ArgumentError(
                String::from("Expecting an argument.")
            ))? {
                True
            } else {
                False
            }),

            Failure => Ok(Exp(args.iter()
                              .map(Expr::to_string)
                              .collect::<Vec<_>>()
                              .join(" "))),

            Do => Ok(args.last().map(Expr::to_owned).unwrap_or(Void)),

            Equals => if args.len() == 0 {
                Ok(True)
            } else {
                let head = args.get(0).unwrap();
                Ok(Expr::from_bool(args[1..].iter().all(|e| e == head)))
            },

            IsClosure if args.len() == 1 =>
                if let Closure(_, _, _, _) = args[0] {
                    Ok(True)
                } else {
                    Ok(False)
                },

            IsBoolean if args.len() == 1 => match args[0] {
                True => Ok(True),
                False => Ok(True),
                _ => Ok(False),
            },

            IsString if args.len() == 1 => if let Str(_) = args[0] {
                    Ok(True)
                } else {
                    Ok(False)
                },

            IsException if args.len() == 1 => if let Exp(_) = args[0] {
                    Ok(True)
                } else {
                    Ok(False)
                },

            IsInteger if args.len() == 1 => if let Int(_) = args[0] {
                    Ok(True)
                } else {
                    Ok(False)
                },

            IsOperator if args.len() == 1 => if let Op(_) = args[0] {
                    Ok(True)
                } else {
                    Ok(False)
                },

            IsObject if args.len() == 1 => if let Ref(addr) = args[0] {
                if let Some(Object(_, _)) = self.heap.get(addr) {
                    Ok(True)
                } else {
                    Ok(False)
                }
            } else {
                Ok(False)
            },

            IsVoid if args.len() == 1 => Ok(Expr::from_bool(args[0] == Void)),

            // Native operations
            Native("markdown") => {
                let mut results = Vec::with_capacity(args.len());
                for arg in args {
                    results.push(match arg {
                        Ref(addr) => match self.heap.get(addr) {
                            Some(e) => e.to_string(),
                            None => return Err(Unsupported(addr.to_string())),
                        },
                        e => e.to_string(),
                    });
                }

                let text = results.join("");

                use pulldown_cmark::{Parser, html};

                let parser = Parser::new(&text);
                let mut markdown = String::new();
                html::push_html(&mut markdown, parser);

                Ok(Str(markdown))
            },

            _ => Err(Unsupported(op.to_string()))
        }
    }

    fn eval_lambda(&mut self, mut arg_exprs: Vec<Expr>) -> EvalResult<Expr> {
        if arg_exprs.len() != 2 {
            return Err(OtherError(String::from(
                "A 'lambda' takes two arguments: an argument specification, and the function body."
            )));
        }

        let arg = match arg_exprs.remove(0) {
            App(fb, mut abs) => {
                abs.insert(0, *fb);

                let mut xs = Vec::with_capacity(abs.len());
                for xe in abs {
                    xs.push(match xe {
                        Var(x) => Ok(x),
                        _ => Err(ArgumentError(format!(
                            "Expecting a variable, got {} instead.",
                            xe
                        ))),
                    }?.to_owned());
                }

                Ok(Arg::Individual(xs))
            },
            Var(x) => Ok(Arg::Collected(x)),
            Nil => Ok(Arg::Individual(vec![])),
            _ => Err(ArgumentError(String::from(
                "Expecting an argument specification"
            ))),
        }?;

        let e = arg_exprs.remove(0);

        let fvs = BTreeSet::from_iter(
            e.free_variables().difference(&BTreeSet::from_iter(
                arg.bound().into_iter().map(str::to_string)
            )).map(String::to_owned));
        let clos_ctx = self.context.condense(&fvs);

        Ok(Closure(None, clos_ctx, arg, Box::new(e.clone())))
    }

    pub fn eval_to_result(mut self, expr: Expr) -> EvalResult<Evaluation> {
        let expr = self.eval(expr)?;
        Ok(Evaluation {
            expr,
            cycles: self.cycles,
            context: self.context,
            heap: self.heap,
        })
    }

    pub fn eval(&mut self, expr: Expr) -> EvalResult<Expr> {
        self.stack_eval(App(
            Box::new(Op(Require)),
            vec![Str(PRELUDE_ID.to_string())]
        ))?;
        self.stack_eval(expr)
    }

    fn stack_eval(&mut self, expr: Expr) -> EvalResult<Expr> {
        #[derive(Clone, Debug, PartialEq, Eq)]
        enum ReturnFrame {
            Embrace,
            RestoreContext,
            ReturnFromOther,
            RememberRequire(String),
            MakeRequireAvailable,
            CloseScope,
            AfterFun(Vec<Expr>),
            Application(usize, Expr),

            Data(Expr),
            EmbraceData(Expr),

            // Special operators
            OpTypeset,
            OpDefineOrSet(bool, String),
            OpIf(Expr, Expr),
            OpLet(Vec<String>, Expr),
            OpCluster(Vec<String>),
            OpExtract(String, bool),
            OpMutation(bool, Vec<String>),

            OpProvide(Vec<String>),
        }

        use ReturnFrame::*;

        let mut data_stack:   Vec<Expr>        = Vec::with_capacity(2000);
        let mut return_stack: Vec<ReturnFrame> = Vec::with_capacity(2000);
        let mut frames = vec![0];
        let mut to_make_available = vec![BTreeSet::new()];

        macro_rules! open_frame {
            { } => { { frames.push(0); } };
        }
        macro_rules! close_frame {
            { } => { {
                let count = frames.pop().unwrap();
                *frames.last_mut().unwrap() += count;
            } };
        }
        macro_rules! pushd {
            ( $e:expr ) => { {
                match $e as EvalResult<_> {
                    Ok(x) => {
                        *frames.last_mut().unwrap() += 1;
                        data_stack.push(x);
                    },
                    Err(x) => {
                        unwind_the_world_and_your_nightmares_gone!();
                        data_stack.push(Exp(x.to_string()));
                        *frames.last_mut().unwrap() += 1;
                    }
                }
            } };
        }
        macro_rules! popd {
            { } => { {
                *frames.last_mut().unwrap() -= 1;
                data_stack.pop().ok_or(InternalError(
                    "Data stack underflowed."
                ))?
            } };
        }
        macro_rules! pushk {
            ( $e:expr ) => { {
                return_stack.push( $e );
            } };
        }
        macro_rules! embrace {
            { } => { {
                pushk!(Embrace);
                open_frame!();
            } };
        }
        macro_rules! popk {
            { } => { {
                return_stack.pop().ok_or(InternalError(
                    "Return stack underflowed."
                ))?
            } };
        }
        macro_rules! unwind_the_world_and_your_nightmares_gone {
            { } => {
                while !return_stack.is_empty() {
                    match popk!() {
                        Embrace => {
                            for _ in 0..frames.pop().unwrap() {
                                data_stack.pop();
                            }
                            break;
                        },
                        RestoreContext => {
                            self.context = self.context_stack.pop().unwrap();
                            self.available_required_stack.pop();
                        },
                        ReturnFromOther => {
                            self.current_item_stack.pop().unwrap();
                        },
                        MakeRequireAvailable => {
                            self.available_required_stack.last_mut()
                                .unwrap()
                                .append(&mut to_make_available.pop().unwrap());
                        },
                        CloseScope => self.context.close(),
                        _ => (),
                    }
                }
            };
        }

        pushk!(Data(expr));

        'primary: while !return_stack.is_empty() {
            self.cycles += 1;

            if self.should_collect_garbage() {
                let mut in_use = BTreeSet::new();
                for f in return_stack.iter() {
                    match f {
                        AfterFun(es) => {
                            es.iter().for_each(
                                |e| in_use.append(&mut e.references())
                            );
                        },

                        OpIf(e1, e2) => {
                            in_use.append(&mut e1.references());
                            in_use.append(&mut e2.references());
                        },
                        Application(_, e) | Data(e) | OpLet(_, e) => {
                            in_use.append(&mut e.references());
                        },

                        _ => (),
                    }
                }
                for e in data_stack.iter() {
                    in_use.append(&mut e.references());
                }
                self.collect_garbage(
                    &mut in_use.iter().map(usize::to_owned)
                );
            }

            if self.stop() {
                return Err(Timeout);
            }

            match popk!() {
                Embrace => close_frame!(),

                RestoreContext => {
                    self.context = self.context_stack.pop().unwrap();
                    self.available_required_stack.pop();
                },
                ReturnFromOther => {
                    self.current_item_stack.pop().unwrap();
                },

                EmbraceData(e) => {
                    pushk!(Embrace);
                    open_frame!();
                    pushk!(Data(e));
                },

                Data(Object(_, _)) => panic!("How did an object get here?"),

                Data(Op(This)) => pushd!(Ok(Str(
                    self.current_item().clone()
                ))),
                Data(e) if e.is_normal() => pushd!(Ok(e)),
                Data(Var(x)) => pushd!(
                    self.get_variable_value(&x)
                        .map(Expr::clone)
                        .or(OpCode::from_str(&x).map(Op))
                        .map(|e| if e == Op(This) {
                            Str(self.current_item().clone())
                        } else {
                            e
                        })
                        .ok_or(Undefined(x))
                ),
                Data(App(fun_box, arg_exprs)) => {
                    pushk!(AfterFun(arg_exprs));
                    pushk!(Data(*fun_box));
                },

                AfterFun(mut arg_exprs) => match popd!() {
                    Op(o@Text) | Op(o@Typeset) => {
                        arg_exprs.reverse();
                        pushk!(Application(arg_exprs.len(), Op(Text)));
                        for arg_expr in arg_exprs {
                            if o == Typeset {
                                pushk!(OpTypeset);
                            }
                            pushk!(EmbraceData(arg_expr));
                        }
                    },

                    Op(Lambda) => {
                        let f = match self.eval_lambda(arg_exprs) {
                            Ok(f) => f,
                            e => {
                                pushd!(e);
                                continue;
                            }
                        };

                        let addr = self.heap.insert(&f);

                        pushd!(Ok(Ref(addr)));
                    },
                    Op(If) => if arg_exprs.len() == 3 {
                        let c = arg_exprs.remove(0);
                        pushk!(OpIf(arg_exprs.remove(0), arg_exprs.remove(0)));
                        pushk!(Data(c));
                    } else {
                        pushd!(Err(ArgumentError(String::from(
                            "An 'if' expression takes three arguments."
                        ))));
                    },
                    Op(Let) => {
                        let (e, bindings) = match arg_exprs.split_last() {
                            Some(p) => p,
                            None => {
                                pushd!(Err(ArgumentError(String::from(
                                    "A 'let' expression takes at least one argument."
                                ))));
                                continue;
                            },
                        };
                        let (xs, mut es) = match parse_bindings(bindings) {
                            Ok(p) => p,
                            Err(e) => {
                                pushd!(Err(e));
                                continue;
                            },
                        };

                        pushk!(OpLet(xs, e.clone()));
                        es.reverse();
                        for e in es {
                            pushk!(Data(e));
                        }
                    },
                    Op(Cluster) => {
                        if arg_exprs.len() < 1 {
                            pushd!(Err(ArgumentError(String::from(
                                "Object creation takes at least one arguments."
                            ))));
                            continue;
                        }

                        let first = arg_exprs.remove(0);
                        let (xs, mut es) = match parse_bindings(&arg_exprs) {
                            Ok(p) => p,
                            Err(e) => {
                                pushd!(Err(e));
                                continue;
                            },
                        };

                        pushk!(OpCluster(xs));
                        es.reverse();
                        for e in es {
                            pushk!(Data(e));
                        }
                        pushk!(Data(first));
                    },
                    Op(op@Extract) | Op(op@HasAttr) => if arg_exprs.len() == 2 {
                        let o = arg_exprs.remove(0);
                        let a = arg_exprs.remove(0);
                        let x = if let Var(x) = a {
                            x
                        } else {
                            pushd!(Err(ArgumentError(format!(
                                "Expecting an attribute name, got {} instead.",
                                a
                            ))));
                            continue;
                        };
                        pushk!(OpExtract(x, op == Extract));
                        pushk!(Data(o));
                    } else {
                        pushd!(Err(ArgumentError(String::from(
                            if op == Extract {
                                "Attribute extraction takes two arguments."
                            } else {
                                "Attribute detection takes two arguments."
                            }
                        ))));
                    },
                    Op(Apply) if arg_exprs.len() >= 2 => {
                        let x = self.fresh();
                        let o = arg_exprs.remove(0);
                        let a = arg_exprs.remove(0);
                        arg_exprs.insert(0, x.clone());
                        pushk!(Data(App(
                            Box::new(Op(Let)),
                            vec![App(Box::new(x.clone()), vec![o]),
                                 App(Box::new(App(Box::new(Op(Extract)),
                                                  vec![x.clone(), a])),
                                     arg_exprs)]
                        )))
                    },
                    Op(op@Mitosis) | Op(op@Update) => if arg_exprs.len() >= 1 {
                        let o = arg_exprs.remove(0);
                        let (xs, mut es) = match parse_bindings(&arg_exprs) {
                            Ok(p) => p,
                            Err(e) => {
                                pushd!(Err(e));
                                continue;
                            },
                        };

                        pushk!(OpMutation(op == Mitosis, xs));
                        es.reverse();
                        for e in es {
                            pushk!(Data(e));
                        }
                        pushk!(Data(o));
                    } else {
                        pushd!(Err(ArgumentError(String::from(if op == Mitosis {
                            "Creating object from new object takes at least one arguments."
                        } else {
                            "Mutating object from new object takes at least one arguments."
                        }))));
                    },
                    Op(o@Define) | Op(o@Set) => if arg_exprs.len() == 2 {
                        if let Var(x) = arg_exprs.remove(0) {
                            pushk!(OpDefineOrSet(o == Define, x));
                            pushk!(Data(arg_exprs.remove(0)));
                        } else {
                            pushd!(Err(TypeError("variable")));
                        }
                    } else {
                        pushd!(Err(ArgumentError(String::from(
                            if o == Define {
                                "Definition takes two arguments."
                            } else {
                                "Assignment takes two arguments."
                            }
                        ))));
                    },

                    Op(Attempt) => if arg_exprs.len() == 1 {
                        pushk!(EmbraceData(arg_exprs.remove(0)));
                    } else {
                        pushd!(Err(ArgumentError(String::from(
                            "Expecting the expression to attempt."
                        ))));
                    },

                    Op(ProvideRequired) =>
                        if let Some(Str(i)) = arg_exprs.get(0) {
                            if self.is_required_available(i) {
                                pushd!(Ok(Void));
                                if !self.required.contains_key(self.current_item()) {
                                    self.required.insert(
                                        self.current_item().clone(),
                                        HashMap::new()
                                    );
                                }
                                let mut to_provide = Vec::new();
                                for (x, v) in self.required[i].iter() {
                                    to_provide.push((x.clone(), v.clone()));
                                }
                                for (x, v) in to_provide {
                                    self.required.get_mut(&self.current_item().clone())
                                        .unwrap()
                                        .insert(x, v);
                                }
                            } else {
                                pushd!(Err(ArgumentError(format!(
                                    "Must require '{}' before providing its content.",
                                    i
                                ))));
                            }
                        } else {
                            pushd!(Err(ArgumentError(String::from(
                                "Expecting an ID as the argument."
                            ))));
                        },

                    Op(Provide) => {
                        let mut xs = Vec::new();
                        for e in arg_exprs.iter() {
                            if let Var(x) = e {
                                xs.push(x.clone());
                            } else {
                                pushd!(Err(ArgumentError(String::from(
                                    "Expecting the variable to provide."
                                ))));
                                continue 'primary;
                            }
                        }

                        pushk!(OpProvide(xs));
                        arg_exprs.reverse();
                        for arg_expr in arg_exprs {
                            pushk!(Data(arg_expr));
                        }
                    },

                    f => {
                        arg_exprs.reverse();
                        pushk!(Application(arg_exprs.len(), f));
                        for arg_expr in arg_exprs {
                            pushk!(Data(arg_expr));
                        }
                    },
                },

                Application(arg_count, fun) => {
                    let mut args = Vec::with_capacity(arg_count);
                    for _ in 0..arg_count {
                        args.insert(0, popd!());
                    }

                    match fun {
                        Op(Require) => if let Some(Str(i)) = args.get(0) {
                            if self.required.contains_key(i) {
                                pushd!(Ok(self.required[i][" "].clone()));
                                pushk!(MakeRequireAvailable);
                                to_make_available.push(BTreeSet::new());
                                to_make_available.last_mut()
                                    .unwrap()
                                    .insert(i.clone());
                            } else {
                                let ast = self.lookup(i).ok_or(
                                    OtherError(format!(
                                        "[{}] is not found.",
                                        i
                                    )))
                                    .and_then(
                                        |input| parse(&input).map_err(
                                            |e| OtherError(format!(
                                                "Encountered an error when parsing [{}]: {}",
                                                i,
                                                e
                                            ))));
                                let expr = match ast {
                                    Ok(a) => speak(&a),
                                    Err(e) => {
                                        pushd!(Err(e));
                                        continue;
                                    },
                                };

                                self.context_stack.push(self.context.clone());
                                self.context = Context::new();
                                self.current_item_stack.push(i.clone());
                                self.available_required_stack.push(BTreeSet::new());

                                pushk!(RememberRequire(i.clone()));
                                pushk!(MakeRequireAvailable);
                                to_make_available.push(BTreeSet::new());
                                to_make_available.last_mut()
                                    .unwrap()
                                    .insert(i.clone());
                                pushk!(ReturnFromOther);
                                pushk!(RestoreContext);

                                pushk!(Data(expr));
                            }
                        } else {
                            pushd!(Err(ArgumentError(String::from(
                                "Expecting an ID as the argument."
                            ))));
                        },

                        Op(Interweave) => if let Some(Str(i)) = args.get(0) {
                            let ast = self.lookup(i).ok_or(
                                OtherError(format!(
                                    "[{}] is not found.",
                                    i
                                )))
                                .and_then(
                                    |input| parse(&input).map_err(
                                        |e| OtherError(format!(
                                            "Encountered an error when parsing [{}]: {}",
                                            i,
                                            e
                                        ))));
                            let expr = match ast {
                                Ok(a) => speak(&a),
                                Err(e) => {
                                    pushd!(Err(e));
                                    continue;
                                },
                            };

                            self.current_item_stack.push(i.clone());
                            pushk!(ReturnFromOther);
                            pushk!(Data(expr));
                        } else {
                            pushd!(Err(ArgumentError(String::from(
                                "Expecting an ID as the argument."
                            ))));
                        },
                        Op(Include) => if let Some(Str(i)) = args.get(0) {
                            if let Some(s) = self.lookup(i) {
                                pushd!(Ok(Str(sanitize(&s))));
                            } else {
                                pushd!(Err(OtherError(format!(
                                    "[{}] is not found.",
                                    i
                                ))))
                            }
                        } else {
                            pushd!(Err(ArgumentError(String::from(
                                "Expecting an ID as the argument."
                            ))));
                        },
                        Op(Exec) => if let Some(Str(i)) = args.get(0) {
                            match parse(&format!("#!\n{}", i)) {
                                Ok(e) => pushk!(Data(speak(&e))),
                                Err(e) => pushd!(Err(OtherError(format!(
                                    "Encountered an error when parsing string: {}",
                                    e
                                )))),
                            }
                        } else {
                            pushd!(Err(ArgumentError(String::from(
                                "Expecting an ID as the argument."
                            ))));
                        },
                        Op(ApplyTo) if arg_count == 2 => {
                            let f = args.remove(0);
                            if let Some(l) = args.remove(0).to_list() {
                                pushk!(AfterFun(l));
                                pushd!(Ok(f));
                            } else {
                                pushd!(Err(ArgumentError(String::from(
                                    "'apply' takes two arguments."
                                ))));
                            }
                        },
                        Op(op) => pushd!(self.apply_op(op, args)),
                        Ref(addr) => match self.heap.get(addr) {
                            Some(Closure(_, _, arg, _)) if !arg.accepts(&args) =>
                                return Err(ArgumentError(format!(
                                    "Expecting {} arguments, got {}",
                                    arg.size(),
                                    args.len(),
                                ))),
                            Some(Closure(_, ctx, arg, e)) => {
                                self.context.push_front(&ctx);
                                match arg {
                                    Arg::Collected(x) => {
                                        let args_as_list = Expr::from_slice(&args);
                                        self.context.define(&x, &args_as_list);
                                    },
                                    Arg::Individual(xs) => {
                                    for (x, arg) in xs.iter().zip(args.iter()) {
                                        self.context.define(x, arg);
                                    }
                                    },
                                }
                                pushk!(CloseScope);

                                pushk!(Data(*e.clone()));
                            },

                            Some(_) => {
                                pushd!(Err(TypeError("procedure")));
                                continue;
                            }
                            None => {
                                pushd!(Err(Undefined(addr.to_string())));
                                continue;
                            }
                        },

                        _ => {
                            pushd!(Err(Unsupported(fun.to_string())));
                            continue;
                        }
                    }
                },

                CloseScope => self.context.close(),

                RememberRequire(iid) => {
                    if !self.required.contains_key(&iid) {
                        self.required.insert(
                            iid.clone(),
                            HashMap::new()
                        );
                    }
                    let e = data_stack.last().unwrap().clone();
                    self.required.get_mut(&iid)
                        .unwrap()
                        .insert(String::from(" "), e);
                },

                MakeRequireAvailable => {
                    self.available_required_stack.last_mut()
                        .unwrap()
                        .append(&mut to_make_available.pop().unwrap());
                },

                Data(e) => return Err(Unsupported(e.to_string())),


                // Operators
                OpTypeset => match popd!() {
                    Ref(addr) => match self.heap.get(addr) {
                        Some(Object(_, _)) => {
                            embrace!();
                            pushk!(AfterFun(
                                vec![Ref(addr), Var(String::from("~"))]
                            ));
                            pushd!(Ok(Op(Apply)))
                        },
                        Some(e) => pushd!(Ok(Str(e.to_string()))),
                        None => {
                            pushd!(Err(InternalError(
                                "Corrupted reference."
                            )));
                            continue;
                        },
                    },
                    e => pushd!(Ok(Str(e.to_string()))),
                },
                OpIf(et, ef) => if let False = popd!() {
                    pushk!(Data(ef));
                } else {
                    pushk!(Data(et));
                },
                OpLet(xs, e) => {
                    let mut es = Vec::with_capacity(xs.len());
                    for _ in 0..xs.len() {
                        es.insert(0, popd!());
                    }

                    self.context.open();
                    for (x, e) in xs.iter().zip(es.iter()) {
                        self.context.define(x, e);
                    }
                    pushk!(CloseScope);

                    pushk!(Data(e));
                },
                OpCluster(xs) => {
                    let mut es = Vec::with_capacity(xs.len());
                    for _ in 0..xs.len() {
                        es.insert(0, popd!());
                    }
                    let s = match popd!() {
                        False | Nil => None,
                        e => Some(Box::new(e)),
                    };

                    let mut internal = BTreeMap::new();
                    for (x, e) in xs.iter().zip(es.iter()) {
                        internal.insert(x.to_string(), e.clone());
                    }

                    let o = Object(s, internal);
                    let addr = self.heap.insert(&o);

                    pushd!(Ok(Ref(addr)));
                },
                OpExtract(x, fail) => {
                    let d = popd!();
                    let (s, bindings) = match d {
                        Ref(addr) => match self.heap.get(addr) {
                            Some(Object(s, bindings)) => (s, bindings),
                            _ => {
                                pushd!(Err(InternalError(
                                    "Corrupted reference."
                                )));
                                continue;
                            },
                        },
                        _ => {
                            pushd!(Err(TypeError("object")));
                            continue;
                        },
                    };

                    if let Some(e) = bindings.get(&x) {
                        pushd!(Ok(e.clone()));
                    } else if let Some(s) = s {
                        pushk!(OpExtract(x, fail));
                        pushk!(Data(*s.clone()));
                    } else if x == "~" {
                        pushd!(Ok(Op(Text)));
                    } else if fail {
                        pushd!(Err(NotFound(x)));
                    } else {
                        pushd!(Ok(False));
                    }
                },
                OpMutation(true, xs) => {
                    let mut es = Vec::with_capacity(xs.len());
                    for _ in 0..xs.len() {
                        es.insert(0, popd!());
                    }
                    let (s, mut bindings) = match popd!() {
                        Ref(addr) => match self.heap.get(addr) {
                            Some(Object(s, bindings)) =>
                                (s.clone(), bindings.clone()),
                            _ => {
                                pushd!(Err(InternalError(
                                    "Corrupted reference."
                                )));
                                continue;
                            },
                        },
                        _ => {
                            pushd!(Err(TypeError("object")));
                            continue;
                        },
                    };

                    for (x, e) in xs.into_iter().zip(es.into_iter()) {
                        bindings.insert(x, e);
                    }

                    let o = Object(s, bindings);
                    let addr = self.heap.insert(&o);

                    pushd!(Ok(Ref(addr)));
                },
                OpMutation(false, xs) => {
                    let mut es = Vec::with_capacity(xs.len());
                    for _ in 0..xs.len() {
                        es.insert(0, popd!());
                    }
                    let bindings = match popd!() {
                        Ref(addr) => match self.heap.get_mut(addr) {
                            Some(Object(_, bindings)) => bindings,
                            _ => {
                                pushd!(Err(InternalError(
                                    "Corrupted reference."
                                )));
                                continue;
                            },
                        },
                        _ => {
                            pushd!(Err(TypeError("object")));
                            continue;
                        },
                    };

                    for (x, e) in xs.into_iter().zip(es.into_iter()) {
                        bindings.insert(x, e);
                    }

                    pushd!(Ok(Void));
                },
                OpDefineOrSet(define, x) => {
                    if define {
                        self.context.define(&x, &popd!());
                    } else {
                        self.context.set(&x, &popd!());
                    }
                    pushd!(Ok(Void));
                },

                OpProvide(xs) => {
                    let mut vs = Vec::with_capacity(xs.len());
                    for _ in 0..xs.len() {
                        vs.insert(0, popd!());
                    }
                    if !self.required.contains_key(self.current_item()) {
                        self.required.insert(
                            self.current_item().clone(),
                            HashMap::new()
                        );
                    }

                    for (x, v) in xs.into_iter().zip(vs.into_iter()) {
                        self.required.get_mut(&self.current_item().clone())
                            .unwrap()
                            .insert(x, v);
                    }

                    pushd!(Ok(Void));
                },
            }
        }

        Ok(popd!())
    }

    fn should_collect_garbage(&self) -> bool {
        self.heap.size() > 1000
    }

    fn collect_garbage(&mut self, in_use: &mut dyn Iterator<Item=Addr>) {
        use std::collections::VecDeque;

        self.heap.unflag_all();

        let mut to_visit = VecDeque::with_capacity(1000);

        macro_rules! push {
            ( $addr: expr) => { {
                if !self.heap.has_flagged($addr) {
                    to_visit.push_back($addr);
                    self.heap.flag($addr);
                }
            } };
        }

        for addr in in_use {
            push!(addr);
        }
        for addr in self.context.references() {
            push!(addr)
        }
        for ctx in self.context_stack.iter() {
            for addr in ctx.references() {
                push!(addr)
            }
        }
        for table in self.required.values() {
            for expr in table.values() {
                for addr in expr.references() {
                    push!(addr)
                }
            }
        }

        while let Some(addr) = to_visit.pop_front() {
            self.heap.flag(addr);
            if let Some(e) = self.heap.get(addr) {
                for addr in e.references() {
                    push!(addr)
                }
            }
        }

        self.heap.remove_all_unflagged();
    }
}


