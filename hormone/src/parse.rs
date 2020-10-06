// Generic parser

use crate::syntax::*;

use crate::sanitize::sanitize;


#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    BadReader,
    BadReference(Srcloc),

    BadAscension(Srcloc),
    InsufficientAscension,

    CharacterFlaw(Srcloc),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BadReader => write!(f, "Bad reader specification."),
            BadReference(loc) => write!(f, "Bad reference format at {:?}.", loc),
            BadAscension(loc) => write!(f, "Bad ascension at {:?}.", loc),
            CharacterFlaw(loc) => write!(f, "Cannot handle character at {:?}.", loc),
            InsufficientAscension => write!(f, "Not enough closing brackets."),
        }
    }
}

impl std::error::Error for ParseError { }


use ParseError::*;


fn is_atom_character(c: char) -> bool {
    c == '-'
        || c == '='
        || c == '+'
        || c == '*'
        || c == '/'
        || c == '<'
        || c == '>'
        || c == '%'
        || c == '&'
        || c == '^'
        || c == '!'
        || c == '@'
        || c == '~'
        || c == '.'
        || c == '?'
        || c.is_alphanumeric()
}


pub fn parse(input: &str) -> Result<Ast, ParseError> {
    fn push_char(ast: Ast, c: char) -> Ast {
        match ast {
            /*
            Ast::Quote(srcloc, body)
                => Ast::Quote(srcloc, Box::new(push_char(*body, c))),
            */
            Ast::RawText(srcloc, mut text)
                => Ast::RawText(srcloc, { text.push(c); text }),
            Ast::Text(srcloc, mut text)
                => Ast::Text(srcloc, { text.push(c); text }),
            Ast::Atom(srcloc, mut atom)
                => Ast::Atom(srcloc, { atom.push(c); atom }),
            Ast::List(srcloc, mut elems)
                => Ast::List(srcloc,
                             { elems.push(Ast::Atom(srcloc, c.to_string()));
                               elems }),
            Ast::Fabric(srcloc, mut elems)
                => Ast::Fabric(srcloc,
                               { elems.push(Ast::Text(srcloc, c.to_string()));
                                 elems }),
        }
    }

    fn push_ast(ast: Ast, to_push: Ast) -> Ast {
        match ast {
            /*
            Ast::Quote(srcloc, body)
                => Ast::Quote(srcloc, Box::new(push_ast(*body, to_push))),
            */
            Ast::List(srcloc, mut elems)
                => Ast::List(srcloc, { elems.push(to_push); elems }),
            Ast::Fabric(srcloc, mut elems)
                => Ast::Fabric(srcloc, { elems.push(to_push); elems }),
            _ => panic!("This should never happen."),
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    enum State {
        Fabric,
        Text,
        Escalation,
        Hash,
        /*
        Quotation,
        */
        Atom,
        List,
        String,
        Escaped,
        AfterEscaped,
        Link,
        Comment,
    }

    let (text_mode, reader, input) = {
        let first_line = input.lines().next().unwrap_or("");

        if first_line.starts_with("#!") || first_line.starts_with("#~") {
            let possible_reader = first_line[2..].trim();
            if possible_reader.chars().all(is_atom_character) {
                (
                    first_line.chars().nth(1) == Some('~'),
                    if possible_reader.len() > 0 {
                        Some(Some(possible_reader.to_string()))
                    } else {
                        Some(None)
                    },
                    if input.len() <= first_line.len() {
                        ""
                    } else {
                        &input[(first_line.len() + 1)..]
                    }
                )
            } else {
                return Err(BadReader);
            }
        } else {
            (true, None, input)
        }
    };

    let mut line = 1;
    let mut col = 0;

    let mut state_stack = Vec::new();
    let mut state = if text_mode {
        State::Fabric
    } else {
        State::List
    };

    let mut stack = Vec::new();
    let mut acc = match &reader {
        Some(Some(r)) =>
            Ast::List((line, col), vec![Ast::Atom((line, col), r.clone())]),

        Some(None) if text_mode =>
            Ast::List(
                (line, col),
                vec![Ast::Atom((line, col), String::from("~~~"))]
            ),

        Some(None) =>
            Ast::List(
                (line, col),
                vec![Ast::Atom((line, col), String::from(">>"))]
            ),

        None =>
            Ast::Fabric((line, col), Vec::new()),
    };

    macro_rules! pop_stack {
        { } => { (stack.pop().ok_or(BadAscension((line, col)))?) }
    }

    macro_rules! pop_state_stack {
        { } => { (state_stack.pop().ok_or(BadAscension((line, col)))?) }
    }

    for c in input.chars() {
        match (&state, c) {
            (State::Fabric, '\\') => {
                state_stack.push(state);
                state = State::Escalation;
            },
            (State::Fabric, c) => {
                state_stack.push(state);
                state = State::Text;
                stack.push(acc);
                acc = Ast::Text((line, col), c.to_string());
            },

            (State::Text, '\\') => {
                acc = push_ast(pop_stack!(), acc);

                state = State::Escalation;
            },
            (State::Text, c)
                => acc = push_char(acc, c),

            (State::Escalation, '(') => {
                state = State::List;
                stack.push(acc);
                acc = Ast::List((line, col), Vec::new());
            },
            (State::Escalation, '[') => {
                state = State::Link;
                stack.push(acc);
                acc = Ast::Text((line, col), String::new());
            },
            (State::Escalation, c) if is_atom_character(c) => {
                state = State::Atom;
                stack.push(acc);
                acc = Ast::Atom((line, col), c.to_string());
            },
            (State::Escalation, '\\') => {
                state = pop_state_stack!();
                acc = push_ast(acc,
                               Ast::Text((line, col), String::from("\\")));
            },
            (State::Escalation, '"') => {
                state = State::String;
                stack.push(acc);
                acc = Ast::Text((line, col), String::new());
            },
            (State::Escalation, '#') => {
                state = State::Hash;
            },
            /*
            (State::Escalation, '\'') => {
                state = State::Quotation;
            },
            */
            (State::Escalation, _)
                => return Err(CharacterFlaw((line, col))),

            (State::Hash, '"') => {
                state = State::String;
                stack.push(acc);
                acc = Ast::RawText((line, col), String::new());
            },
            (State::Hash, _)
                => return Err(CharacterFlaw((line, col))),

            /*
            (State::Quotation, '(') => {
                state = State::List;
                stack.push(acc);
                acc = Ast::Quote(
                    (line, col),
                    Box::new(Ast::List((line, col), Vec::new()))
                );
            },
            (State::Quotation, '[') => {
                state = State::Link;
                stack.push(acc);
                acc = Ast::Quote(
                    (line, col),
                    Box::new(Ast::Text((line, col), String::new()))
                );
            },
            (State::Quotation, c) if is_atom_character(c) => {
                state = State::Atom;
                stack.push(acc);
                acc = Ast::Quote(
                    (line, col),
                    Box::new(Ast::Atom((line, col), c.to_string()))
                );
            },
            (State::Quotation, '"') => {
                state = State::String;
                stack.push(acc);
                acc = Ast::Quote(
                    (line, col),
                    Box::new(Ast::Text((line, col), String::new()))
                );
            },
            (State::Quotation, '\'') => {
                acc = Ast::Quote((line, col), Box::new(acc));
            },
            (State::Quotation, _)
                => return Err(CharacterFlaw((line, col))),
            */

            (State::Atom, ';') => {
                acc = push_ast(pop_stack!(), acc);
                state = State::Comment;
            },
            (State::Atom, '(') => {
                acc = push_ast(pop_stack!(), acc);

                state = State::List;
                stack.push(acc);
                acc = Ast::List((line, col), Vec::new());
            },
            (State::Atom, '[') => {
                acc = push_ast(pop_stack!(), acc);

                state = State::Link;
                stack.push(acc);
                acc = Ast::Text((line, col), String::new());
            },
            /*
            (State::Atom, '\'') => {
                acc = push_ast(pop_stack!(), acc);

                state = State::Quotation;
            },
            */
            (State::Atom, ')') => {
                pop_state_stack!();
                state = pop_state_stack!();
                acc = push_ast(pop_stack!(), acc);
                acc = push_ast(pop_stack!(), acc);
            },
            (State::Atom, ws) if ws.is_whitespace() => {
                state = pop_state_stack!();
                acc = push_ast(pop_stack!(), acc);
            },
            (State::Atom, c) if is_atom_character(c)
                => acc = push_char(acc, c),
            (State::Atom, _)
                => return Err(CharacterFlaw((line, col))),

            (State::List, ';') => {
                state_stack.push(state);
                state = State::Comment;
            },
            (State::List, '\\') => {
                acc = push_ast(acc, Ast::Atom((line, col), String::from("\\")));
            },
            (State::List, '(') => {
                state_stack.push(state);
                state = State::List;
                stack.push(acc);
                acc = Ast::List((line, col), Vec::new());
            },
            (State::List, '[') => {
                state_stack.push(state);
                state = State::Link;
                stack.push(acc);
                acc = Ast::Text((line, col), String::new());
            },
            /*
            (State::List, '\'') => {
                state_stack.push(state);
                state = State::Quotation;
            },
            */
            (State::List, ')') => {
                state = pop_state_stack!();
                acc = push_ast(pop_stack!(), acc);
            },
            (State::List, ws) if ws.is_whitespace() => (),
            (State::List, '"') => {
                state_stack.push(state);
                state = State::String;
                stack.push(acc);
                acc = Ast::Text((line, col), String::new());
            },
            (State::List, '#') => {
                state_stack.push(state);
                state = State::Hash;
            },
            (State::List, c) if is_atom_character(c) => {
                state_stack.push(state);
                state = State::Atom;
                stack.push(acc);
                acc = Ast::Atom((line, col), c.to_string());
            },
            (State::List, _)
                => return Err(CharacterFlaw((line, col))),

            (State::String, '"') => {
                state = pop_state_stack!();
                acc = push_ast(pop_stack!(), acc);
            },
            (State::String, '~') => {
                state = State::Escaped;
            },
            (State::String, _)
                => acc = push_char(acc, c),

            (State::Escaped, '\r') | (State::Escaped, '\n')
                | (State::AfterEscaped, '\r') | (State::AfterEscaped, '\n')
                => state = State::AfterEscaped,

            (State::Escaped, _) => {
                state = State::String;
                acc = push_char(acc, match c {
                    '<' => '\n',
                    '"' => '"',
                    _ => c,
                });
            },
            (State::AfterEscaped, _) => {
                state = State::String;
                acc = push_char(acc, c);
            },

            (State::Link, ';') => {
                state_stack.push(state);
                state = State::Comment;
            },
            (State::Link, ']') => {
                state = pop_state_stack!();
                acc = match acc {
                    Ast::Text(srcloc, text) => {
                        let parts = text.split_whitespace()
                            .collect::<Vec<_>>();

                        if parts.len() == 0 || parts.len() > 2 {
                            return Err(BadReference(srcloc));
                        }

                        let op = if parts.len() == 2 {
                            Ast::Atom(srcloc, parts[0].to_string())
                        } else {
                            Ast::Atom(srcloc, String::from("=>"))
                        };

                        let target = if parts.len() == 2 {
                            Ast::Text(srcloc, parts[1].to_string())
                        } else {
                            Ast::Text(srcloc, parts[0].to_string())
                        };

                        Ast::List(srcloc, vec![op, target])
                    },

                    /*
                    Ast::Quote(_, boxed) =>
                        if let Ast::Text(srcloc, text) = *boxed {
                            Ast::Quote(
                                srcloc,
                                Box::new(
                                    Ast::List(
                                        srcloc,
                                        vec![Ast::Atom(srcloc,
                                                       String::from("=>")),
                                             Ast::Text(srcloc, text)]
                                    )))
                        } else {
                            panic!("Impossible")
                        },
                    */

                    _ => panic!("Impossible: {:?}", acc),
                };
                acc = push_ast(pop_stack!(), acc);
            },
            (State::Link, c)
                => acc = push_char(acc, c),

            (State::Comment, '\n') => {
                state = pop_state_stack!();
            },
            (State::Comment, _) => (),
        }

        if c == '\n' {
            line = line + 1;
            col = 0;
        } else {
            col = col + 1;
        }
    }

    // EOF
    match &state {
        State::List if reader.is_some() => (),

        State::Text | State::Atom
            => acc = push_ast(pop_stack!(), acc),

        State::Escalation | State::List
            => return Err(InsufficientAscension),

        _ => (),
    }

    if !stack.is_empty() {
        Err(InsufficientAscension)
    } else {
        Ok(acc)
    }
}


fn str_to_expr(str: &str) -> Expr {
    use Expr::*;

    match str {
        "." => Nil,

        _ => if let Ok(i) = isize::from_str_radix(str, 10) {
            Int(i)
        } else {
            Var(str.to_string())
        },
    }
}

pub fn speak(ast: &Ast) -> Expr {
    use Expr::*;

    match ast {
        /*
        Ast::Quote(_, body)
            => App(Box::new(Op(Quote)), vec![Box::new(speak(body))]),
        */
        Ast::Text(_, text) => Str(sanitize(text)),
        Ast::RawText(_, text) => Str(text.clone()),
        Ast::Atom(_, s) => str_to_expr(&s),
        /*
        Ast::List(_, elems) if elems.is_empty()
            => panic!("Invalid application."),
        */
        Ast::List(_, elems) if elems.is_empty() => Nil,
        Ast::List(_, elems)
            => App(Box::new(speak(&elems[0])),
                   elems[1..].iter().map(speak).collect()),
        Ast::Fabric(_, elems)
            => App(
                Box::new(str_to_expr("~~~")),
                elems.iter()
                    .map(speak)
                    .map(|e| App(Box::new(Op(OpCode::Attempt)), vec![e]))
                    .collect()
            ),
        /*
        Ast::Fabric(_, elems)
            => App(Box::new(Op(Text)),
                   elems.iter().map(speak).collect()),
        */
    }
}

