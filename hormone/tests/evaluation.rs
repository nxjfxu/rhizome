use hormone::*;
use hormone::syntax::Expr;
use hormone::syntax::Expr::*;


fn evaluate_expr_to_expr(program: &str) -> Expr {
    let lookup: Box<LookupFn> = Box::new(|_| Some(String::from("")));
    let ev = evaluate(
        "test",
        &lookup,
        false,
        &format!("#!\n{}", program),
        "",
        &mut std::iter::empty()
    );
    match ev {
        Ok(ev) => ev.expr.clone(),
        _ => panic!("Evaluation of {} failed.", program),
    }
}


macro_rules! expr_evaluates_to_exact {
    ($prog:expr, $e:expr) => {
        assert_eq!(evaluate_expr_to_expr($prog), $e);
    };
}

macro_rules! expr_evaluates_to {
    ($prog:expr, $( $p:pat )|+ $( if $guard: expr )? $(,)?) => {
        assert!(matches!(
            evaluate_expr_to_expr($prog),
            $( $p )|+ $( if $guard: expr )?
        ));
    };
}

#[test]
fn arithmetic_basics() {
    expr_evaluates_to_exact!("(+ 2 3)", Int(5));
    expr_evaluates_to_exact!("(- 2 3)", Int(-1));
    expr_evaluates_to_exact!("(* 2 3)", Int(6));
    expr_evaluates_to_exact!("(/ 2 3)", Int(0));
    expr_evaluates_to_exact!("(> 2 3)", False);
    expr_evaluates_to_exact!("(= 2 3)", False);
    expr_evaluates_to_exact!("(< 2 3)", True);
    expr_evaluates_to_exact!("(>= 2 3)", False);
    expr_evaluates_to_exact!("(<= 2 3)", True);
    expr_evaluates_to_exact!("(/ 0 3)", Int(0));
    expr_evaluates_to_exact!("(/ 0 10)", Int(0));
    expr_evaluates_to_exact!("(/ 10 3)", Int(3));
}

#[test]
fn arithmetic_multi_args() {
    expr_evaluates_to_exact!("(+ 1 2 3 4 5)", Int(15));
    expr_evaluates_to_exact!("(* 1 2 3 4 5)", Int(120));
    expr_evaluates_to_exact!("(* 1 2 -3 4 5)", Int(-120));
    expr_evaluates_to_exact!("(/ 120 1 2 3 4)", Int(5));
    expr_evaluates_to_exact!("(/ 120 -1 2 3 4)", Int(-5));
    expr_evaluates_to_exact!("(- 1 2 3)", Int(-4));
    expr_evaluates_to_exact!("(+)", Int(0));
    expr_evaluates_to_exact!("(-)", Int(0));
    expr_evaluates_to_exact!("(*)", Int(1));
}

#[test]
fn arithmetic_nested() {
    expr_evaluates_to_exact!("(+ (+ 1 2) (+ 3 4))", Int(10));
    expr_evaluates_to_exact!("(* (+ 1 2) (- 3 4))", Int(-3));
    expr_evaluates_to_exact!("(/ (+ 1 2 3) (/ 12 4) (+ 1 1))", Int(1));
    expr_evaluates_to_exact!(
        "(/ (+ 1 2 3) (/ 12 4) (+ 1 1))",
        Int(1)
    );
}

#[test]
fn arithmetic_errors() {
    expr_evaluates_to!("(/)", Exp(_));
    expr_evaluates_to!("(* 123456789123 123456789123)", Exp(_));
    expr_evaluates_to!("(* 12345678912345 -12345678912345)", Exp(_));
    expr_evaluates_to!("(/ 1 0)", Exp(_));
    expr_evaluates_to!("(/ 1 (* 1 2 3 0))", Exp(_));
    expr_evaluates_to!("(* 123 456 789 123 456 789 123 456 789 123 456 789)", Exp(_));
    expr_evaluates_to!("(* 123 -456 789 -123 456 789 123 456 789 123 456 789)", Exp(_));
    expr_evaluates_to!("(/ (123 -456 789 -123 456 789 123 456 789 123 456) 789)", Exp(_));
    expr_evaluates_to!("(* (123 -456 789 -123 456 789 123 456 789 123 456) 1)", Exp(_));
    expr_evaluates_to!("(/ (123 -456 789 -123 456 789 123 456 789 123 456) (- 1 1))", Exp(_));
}


// Boolean

#[test]
fn logic_basics() {
    expr_evaluates_to_exact!("(-*)", True);
    expr_evaluates_to_exact!("(-+)", True);
    expr_evaluates_to_exact!("(-* #+)", True);
    expr_evaluates_to_exact!("(-* #-)", False);
    expr_evaluates_to_exact!("(-+ #+)", True);
    expr_evaluates_to_exact!("(-+ #-)", False);
    expr_evaluates_to_exact!("(-* #+ #+)", True);
    expr_evaluates_to_exact!("(-* #+ #-)", False);
    expr_evaluates_to_exact!("(-* #+ #+ #+)", True);
    expr_evaluates_to_exact!("(-* #+ #- #+)", False);
    expr_evaluates_to_exact!("(-+ #+ #+)", True);
    expr_evaluates_to_exact!("(-+ #- #-)", False);
    expr_evaluates_to_exact!("(-+ #+ #-)", True);
    expr_evaluates_to_exact!("(-- #+)", False);
    expr_evaluates_to_exact!("(-- #-)", True);
}

#[test]
fn logic_nested() {
    expr_evaluates_to_exact!("(-* (-* #+ #+) #+)", True);
    expr_evaluates_to_exact!("(-* #+ (-* #+ #+))", True);
    expr_evaluates_to_exact!("(-* (-* #+ #+) #+ (-*))", True);
    expr_evaluates_to_exact!("(-* (-* #- #+) #+ (-*))", False);
    expr_evaluates_to_exact!("(-* (-* #+ #+) (-* #+ #+))", True);
    expr_evaluates_to_exact!("(-* (-+ #- #+) (-* #+ #+))", True);
    expr_evaluates_to_exact!("(-* (-+ #- #-) (-* #+ #+))", False);
    expr_evaluates_to_exact!("(-* (-+ #- #- #+) (-* #+ #+ #-))", False);
    expr_evaluates_to_exact!("(-* (-+ #- #- #+) (-+ #+ #+ #-))", True);
}

#[test]
fn logic_and_other_expressions() {
    expr_evaluates_to_exact!("(-- .)", False);
    expr_evaluates_to_exact!("(-- ())", False);
    expr_evaluates_to_exact!("(-- 0)", False);
    expr_evaluates_to_exact!("(-* 100)", Int(100));
    expr_evaluates_to_exact!("(-+ 100)", Int(100));
    expr_evaluates_to_exact!("(-* 100 200)", Int(200));
    expr_evaluates_to_exact!("(-+ 100 200)", Int(100));
    expr_evaluates_to_exact!("(-* 100 200 1 2 3 4)", Int(4));
    expr_evaluates_to_exact!("(-+ 100 200 1 2 3 4)", Int(100));
    expr_evaluates_to_exact!("(-* (* 2 100) 200 1 2 3 (* 4 4))", Int(16));
    expr_evaluates_to_exact!("(-+ (* 2 100) 200 1 2 3 (* 4 4))", Int(200));
    expr_evaluates_to_exact!("(-* 100 #- 1 2 3 4)", False);
    expr_evaluates_to_exact!("(-+ 100 #- 1 2 3 4)", Int(100));
    expr_evaluates_to_exact!("(-+ #- #- 1 2 3 4)", Int(1));
    expr_evaluates_to_exact!("(-* (-- 100) 200)", False);
    expr_evaluates_to_exact!("(-+ (-- 100) 200)", Int(200));
}

#[test]
fn logic_and_comparison() {
    expr_evaluates_to_exact!("(-- (> 1 0))", False);
    expr_evaluates_to_exact!("(-- (> -1 0))", True);
    expr_evaluates_to_exact!("(-+ (> 1 0) (< 10 20) (= 30 30))", True);
    expr_evaluates_to_exact!("(-* (> 1 0) (< 10 20) (= 30 30))", True);
    expr_evaluates_to_exact!("(-+ (> (* 1 0) 0) (< (+ 10 10) 20) (= 30 31))", False);
}

#[test]
fn logic_bypass() {
    // No bypass
    expr_evaluates_to!("(-+ (/ 1 0))", Exp(_));
    expr_evaluates_to!("(-+ (= 0 1) (= 2 3) (/ 1 0))", Exp(_));
    expr_evaluates_to!("(-+ (= 0 1) (= 2 3) (/ 1 0) 4 5)", Exp(_));
    expr_evaluates_to!("(-+ (= 0 1) (= 2 3) (/ 1 0) #-)", Exp(_));
    expr_evaluates_to!("(-* (/ 1 0))", Exp(_));
    expr_evaluates_to!("(-* 1 2 (/ 1 0))", Exp(_));
    expr_evaluates_to!("(-* 1 2 (/ 1 0) 3 4)", Exp(_));
    expr_evaluates_to!("(-* (= 1 1) (= 2 2) (/ 1 0))", Exp(_));
    expr_evaluates_to!("(-* 1 (/ 1 0) #-)", Exp(_));

    // Bypassed
    expr_evaluates_to_exact!("(-+ 1 2 (/ 1 0))", Int(1));
    expr_evaluates_to_exact!("(-+ #- 2 (/ 1 0))", Int(2));
    expr_evaluates_to_exact!("(-* #- 2 (/ 1 0))", False);
    expr_evaluates_to_exact!("(-* 1 #- (/ 1 0))", False);
    expr_evaluates_to_exact!("(-* 1 #- (/ 1 0) #-)", False);
}

#[test]
fn logic_errors() {
    // There is only one possibility...
    expr_evaluates_to!("(--)", Exp(_));
}

// Conditional

#[test]
fn conditional_basics() {
    expr_evaluates_to_exact!("(? #+ 1 2)", Int(1));
    expr_evaluates_to_exact!("(? #- 1 2)", Int(2));
    expr_evaluates_to_exact!("(? 1 1 2)", Int(1));
    expr_evaluates_to_exact!("(? 2 1 2)", Int(1));
    expr_evaluates_to_exact!("(? #+ (+ 1 1) (+ 2 2))", Int(2));
    expr_evaluates_to_exact!("(? #- (+ 1 1) (+ 2 2))", Int(4));
    expr_evaluates_to_exact!("(? (> 1 2) 3 4)", Int(4));
    expr_evaluates_to_exact!("(? (< 1 2) 3 4)", Int(3));
    expr_evaluates_to_exact!("(? (> 1 (* 0 2)) 3 4)", Int(3));
    expr_evaluates_to_exact!("(? (< 1 (* 0 2)) 3 4)", Int(4));
}

#[test]
fn conditional_nested() {
    expr_evaluates_to_exact!("(? #+ (? #+ 1 2) 3)", Int(1));
    expr_evaluates_to_exact!("(? #+ (? #- 1 2) 3)", Int(2));
    expr_evaluates_to_exact!("(? #- (? #- 1 2) 3)", Int(3));
    expr_evaluates_to_exact!("(? #- 1 (? #+ 2 (? #+ 3 4)))", Int(2));
    expr_evaluates_to_exact!("(? #- 1 (? #- 2 (? #+ 3 4)))", Int(3));
    expr_evaluates_to_exact!("(? #- 1 (? #- 2 (? #+ (? #- 3 4) 5)))", Int(4));
    expr_evaluates_to_exact!("(? #+ (? #+ 1 2) (? #- 3 4))", Int(1));
    expr_evaluates_to_exact!("(? #- (? #+ 1 2) (? #- 3 4))", Int(4));
    expr_evaluates_to_exact!("(? (? #+ #- #+) 1 2)", Int(2));
}

#[test]
fn conditional_bypass() {
    // No bypass
    expr_evaluates_to!("(? #+ (/ 1 0) 1)", Exp(_));
    expr_evaluates_to!("(? #- 1 (/ 1 0))", Exp(_));
    expr_evaluates_to!("(? #- 1 (+ 1 2 (/ 1 0)))", Exp(_));

    // Bypassed
    expr_evaluates_to_exact!("(? #+ 1 (/ 1 0))", Int(1));
    expr_evaluates_to_exact!("(? #- (/ 1 0) 2)", Int(2));
    expr_evaluates_to_exact!("(? #+ 1 (+ (/ 1 0) 2 3 4))", Int(1));
}


// Sequence

#[test]
fn sequence_basics() {
    expr_evaluates_to_exact!("(>>)", Void);
    expr_evaluates_to_exact!("(>> 1)", Int(1));
    expr_evaluates_to_exact!("(>> 1 2 3)", Int(3));
    expr_evaluates_to_exact!("(>> (>> 1 2) 3)", Int(3));
    expr_evaluates_to_exact!("(>> (>> 1 2 3))", Int(3));
    expr_evaluates_to_exact!("(>> 1 2 (>> 1 2 3))", Int(3));
    expr_evaluates_to_exact!("(>> 1 2 (+ 1 2 3))", Int(6));
    expr_evaluates_to_exact!("(>> 1 2 ((>> +) 1 (>> 2 3)))", Int(4));

    expr_evaluates_to_exact!("(+ (>> 1 2) (>> 3 4))", Int(6));
    expr_evaluates_to_exact!("((>> + - * /) 20 10)", Int(2));
}


// Assignment

#[test]
fn assignment_basics() {
    expr_evaluates_to_exact!(
        "
(<- x 1)
x
",
        Int(1)
    );
    expr_evaluates_to_exact!(
        "
(<- x 1)
(<- y 2)
(+ x y)
",
        Int(3)
    );
    expr_evaluates_to_exact!(
        "
(<- x 1)
(<- y x)
(+ y y)
",
        Int(2)
    );
    expr_evaluates_to_exact!(
        "
(<- x (+ 1 2))
x
",
        Int(3)
    );
    expr_evaluates_to_exact!(
        "
(<- x (? (> 1 0) 10 20))
(* x x)
",
        Int(100)
    );


    // Re-assignment
    expr_evaluates_to_exact!(
        "
(<- x 1)
(<- x 2)
(* x x)
",
        Int(4)
    );
    expr_evaluates_to_exact!(
        "
(<- x 2)
(<- y 3)
(<- x y)
(+ x y)
",
        Int(6)
    );

    // Mutation
    expr_evaluates_to_exact!(
        "
(<- x 1)
(<! x 2)
x
",
        Int(2)
    );
    expr_evaluates_to_exact!(
        "
(<- x 1)
(<- y 2)
(<! y 3)
(+ x y)
",
        Int(4)
    );
    expr_evaluates_to_exact!(
        "
(<- x 1)
(<! x 2)
(<! x 1)
(+ x x)
",
        Int(2)
    );
    expr_evaluates_to_exact!(
        "
(<- x 1)
(<! x (* x 2))
(<! x (* x 3))
(+ x x)
",
        Int(12)
    );
}

#[test]
fn assignment_undefined() {
    expr_evaluates_to!("x", Exp(_));
    expr_evaluates_to!(
        "
(<- x 10)
y
",
        Exp(_)
    );

    // Cannot mutate before assign
    expr_evaluates_to!(
        "
(<! x 10)
",
        Exp(_)
    );
    expr_evaluates_to!(
        "
(<- x 10)
(<! y 20)
(+ x y)
",
        Exp(_)
    );
    expr_evaluates_to!(
        "
(<! x 10)
(<- x 20)
x
",
        Exp(_)
    );
}

#[test]
fn assignment_scope() {
    // "Sequence" does not open a new scope
    expr_evaluates_to_exact!("(>> (<- x 10)) x", Int(10));
    expr_evaluates_to_exact!("(>> (>> (<- x 10))) x", Int(10));
    expr_evaluates_to_exact!("(>> (<- x 10) (<- y 20)) (* x y)", Int(200));
    expr_evaluates_to_exact!("(<- x 10) (>> (<- y x) (+ x y))", Int(20));

    expr_evaluates_to_exact!("(<- x 10) (>> (<! x 20)) x", Int(20));
    expr_evaluates_to_exact!("(>> (<- x 10) (<! x 21)) x", Int(21));
    expr_evaluates_to_exact!("(>> (<- x 10)) (<! x 22) x", Int(22));
    expr_evaluates_to_exact!("(>> (<- x 10)) (>> (<! x 23)) x", Int(23));
    expr_evaluates_to_exact!(
        "
(>>
 (<- x 10)
 (>> (<- y 20) (<! x 30))
 (<- z (+ x y)))
(>> (>> (>> (<! z (* z 2)))))
(>> z)
",
        Int(100)
    );

    // Conditionals open new scopes
    expr_evaluates_to!(
        "
(? (= 1 1) (<- x 10) (<- x 20))
x
",
        Exp(_)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(? (= 1 1) (<- x 20) (<- x 30))
x
",
        Int(10)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<- y 20)
(? (= 1 1) (<- x 30) (<- y 40))
(+ x y)
",
        Int(30)
    );

    // Mutation will still change defined variable
    expr_evaluates_to_exact!(
        "
(<- x 10)
(? (= 1 1) (<! x 1) (<! y 2))
x
",
        Int(1)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<- y 20)
(? (> 1 2) (<! x (+ x 1)) (<! y (+ y 2)))
(+ x y)
",
        Int(32)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<- y 20)
(? (> 1 2) (<! x (+ x 1)) (>> (<! y (+ y 2)) (<! y (+ y 3))))
(+ x y)
",
        Int(35)
    );

    // Inner scope will be lost
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<- y 20)
(? (> 1 2) (<- x 100) (<- y 200))
(+ x y)
",
        Int(30)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<- y 20)
(<- z (? (> 1 2) (>> (<- x 100) x) (>> (<- y 200) y)))
(+ x y z)
",
        Int(230)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<- y 20)
(<- z (? (> 2 1) (>> (<- x 100) y) (>> (<- y 200) x)))
(+ x y z)
",
        Int(50)
    );

    // Scopes are nested
    expr_evaluates_to!(
        "
(? (> 2 1)
   (>> (? (> 1 2) (<- x 10) (<- x 20))
       x)
   100)
",
        Exp(_)
    );
    expr_evaluates_to_exact!(
        "
(<- x 100)
(? (> 2 1)
   (>> (? (> 1 2) (<- x 10) (<- x 20))
       x)
   100)
",
        Int(100)
    );
    expr_evaluates_to_exact!(
        "
(<- x 100)
(? (> 2 1)
   (>> (<- x 1000)
       (? (> 1 2) (<- x 10) (<- x 20))
       x)
   100)
",
        Int(1000)
    );
    expr_evaluates_to_exact!(
        "
(<- x 100)
(? (> 2 1)
   (>> (<- x 1000)
       (? (> 1 2) (<! x 10) (<! x 20))
       x)
   100)
",
        Int(20)
    );
    expr_evaluates_to_exact!(
        "
(<- x 100)
(? (> 2 1)
   (>> (? (> 1 2) (<! x 10) (<! x 20))
       x)
   100)
",
        Int(20)
    );
}

#[test]
fn assignment_evaluation_order() {
    expr_evaluates_to_exact!(
        "
(<- x 1)
(+ x (>> (<- x 2) x) (>> (<- y x) (<- x 3) y) x)
",
        Int(8)
    );
    expr_evaluates_to_exact!(
        "
(<- x 1)
(+ x (>> (<- y 100) (<- x 2) x) (>> (<- y x) (<- x 3) y) y)
",
        Int(7)
    );
}


// Let bindings

#[test]
fn let_basics() {
    expr_evaluates_to_exact!("(<-- 10)", Int(10));

    expr_evaluates_to_exact!("(<-- (x 10) x)", Int(10));
    expr_evaluates_to_exact!("(<-- (x 10) (x 20) x)", Int(20));
    expr_evaluates_to_exact!("(<-- (x 10) (y 20) x)", Int(10));
    expr_evaluates_to_exact!("(<-- (x 10) (y 20) y)", Int(20));
    expr_evaluates_to_exact!("(<-- (x 10) (y 20) (+ x y))", Int(30));
    expr_evaluates_to_exact!("(<-- (x 10) (y 20) (z 30) (+ x y z))", Int(60));
    expr_evaluates_to_exact!("(<-- (x 10) (y 20) (x 30) (+ x y))", Int(50));

    expr_evaluates_to_exact!("(<-- (x (+ 1 1)) (y (+ 2 2)) (* x y))", Int(8));

    expr_evaluates_to!("(<-- (x (/ 1 0)) (y (+ 2 2)) (* x y))", Exp(_));
    expr_evaluates_to!("(<-- (x (+ 1 1)) (y (/ 2 0)) (* x y))", Exp(_));
}

#[test]
fn let_nested() {
    expr_evaluates_to_exact!("(<-- (x 10) (<-- (y 20) (+ x y)))", Int(30));
    expr_evaluates_to_exact!("(<-- (x 10) (<-- (+ x x)))", Int(20));
    expr_evaluates_to_exact!("(<-- (x (+ 10 (<-- (x 10) x))) x)", Int(20));
    expr_evaluates_to_exact!(
        "(<-- (x (+ 10 (<-- (x 10) x))) (y 20) (+ x y))",
        Int(40)
    );
    expr_evaluates_to_exact!(
        "(<-- (x (+ 10 (<-- (x 10) x))) (<-- (y 30) (+ x y)))",
        Int(50)
    );
    expr_evaluates_to_exact!("(<-- (x 10) (<-- (y 20) (z 30) (+ x y z)))", Int(60));
    expr_evaluates_to_exact!(
        "(<-- (x 10) (<-- (y 20) (<-- (z 30) (+ x y z))))",
        Int(60)
    );
    expr_evaluates_to_exact!(
        "(<-- (x 10) (+ (<-- (y 20) (+ x y)) (<-- (y 30) (+ x y))))",
        Int(70)
    );
}

#[test]
fn let_assignment() {
    expr_evaluates_to_exact!("(<-- (x 10) (>> (<! x 20) x))", Int(20));
    expr_evaluates_to_exact!("(<-- (x 10) (y 20) (>> (<! x 30) x))", Int(30));
    expr_evaluates_to_exact!(
        "(<-- (x 10) (<-- (y 20) (>> (<! x 30) x)))",
        Int(30)
    );
    expr_evaluates_to_exact!(
        "(<-- (x 10) (<-- (y 20) (>> (<! y 30) (+ x y))))",
        Int(40)
    );
    expr_evaluates_to_exact!(
        "(<-- (x 10) (<-- (y 20) (>> (<! x 0) (<! y 30) (+ x y))))",
        Int(30)
    );
}

#[test]
fn let_scope() {
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<-- (y 20) x)
",
        Int(10)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<-- (x 20) x)
(<-- (x 30) x)
",
        Int(30)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<-- (x 20) x)
(<-- (x 30) x)
x
",
        Int(10)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<-- (x 20) x)
",
        Int(20)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<-- (x 20) (>> (<! x 30) x))
",
        Int(30)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<-- (x 20) x)
x
",
        Int(10)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<-- (x 20) (>> (<! x 30) x))
x
",
        Int(10)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<-- (y 20) (>> (<! x 30) x))
x
",
        Int(30)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(<-- (y 20) (>> (<! y 30) x))
x
",
        Int(10)
    );

    expr_evaluates_to_exact!(
        "
(<-- (x 10) (>> (<! x 20) (<-- (x 30) (* x 2))))
",
        Int(60)
    );
    expr_evaluates_to_exact!(
        "
(<-- (x 10) (>> (<! x 20) (<-- (x 30) (* x 2) x) x))
",
        Int(20)
    );
    expr_evaluates_to_exact!(
        "
(<- x 10)
(+ x (<-- (y 20) (>> (<! x 30) (<-- (x 1) (y 2) (+ x y)))))
",
        Int(13)
    );
    expr_evaluates_to_exact!( // Evaluation order also involved
        "
(<- x 10)
(+ (<-- (y 20) (>> (<! x 30) (<-- (x 1) (y 2) (+ x y)))) x)
",
        Int(33)
    );
    expr_evaluates_to_exact!( // Evaluation order also involved
        "
(<- x 10)
(+ x (<-- (y 20) (>> (<! x 30) (<-- (x 1) (y 2) (+ x y)))) x)
",
        Int(43)
    );

    expr_evaluates_to!(
        "
(<-- (x 20) x)
x
",
        Exp(_)
    );
    expr_evaluates_to!(
        "
(<-- (y 20) x)
",
        Exp(_)
    );
    expr_evaluates_to!(
        "
(<-- x)
",
        Exp(_)
    );

    expr_evaluates_to_exact!(
        "
(<-- (x (<-- (y 10) y)) x)
",
        Int(10)
    );
    expr_evaluates_to!(
        "
(<-- (x (<-- (y 10) y)) y)
",
        Exp(_)
    );
}

