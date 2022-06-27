use crate::{
    common::Symbol,
    parser::ast::{Decl, Expr, LValue, VarDecl},
};

use super::env::Env;

type Depth = u32;
pub struct EscapeFinder<'a> {
    esc_env: Env<(Depth, &'a mut bool)>, // (depth, is_escape)
}

impl<'a, 'b> EscapeFinder<'a>
where
    'a: 'b,
{
    pub fn find_escape(expr: &mut Expr) {
        let mut env = Self::new();
        env.traverse_expr(0, expr);
    }

    fn new() -> Self {
        Self {
            esc_env: Env::new(),
        }
    }

    fn new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.esc_env.begin_scope();
        let r = f(self);
        self.esc_env.end_scope();
        r
    }

    fn traverse_var(&'b mut self, depth: Depth, lvalue: &'a LValue) {
        match lvalue {
            LValue::Var(id, _) => {
                let sym = Symbol::from(id);
                if let Some((depth_inner, is_escaped)) = self.esc_env.look_mut(sym) {
                    if *depth_inner < depth {
                        **is_escaped = true
                    }
                }
            }
            LValue::RecordField(lvalue, _, _) => self.traverse_var(depth, lvalue),
            LValue::Array(lvalue, _, _) => self.traverse_var(depth, lvalue),
        }
    }

    fn traverse_decl(&'b mut self, depth: Depth, decl: &'a mut Decl) {
        match decl {
            Decl::Type(_) => (),
            Decl::Var(VarDecl(ref id, is_escape, _, expr, _)) => {
                let sym = Symbol::from(id);
                self.esc_env.enter(sym, (depth, is_escape));
                self.traverse_expr(depth, expr);
            }
            Decl::Func(fn_decls) => {
                for fn_decl in fn_decls {
                    self.new_scope(|_self| {
                        for arg in &mut fn_decl.params {
                            let sym = Symbol::from(&arg.id);
                            _self.esc_env.enter(sym, (depth + 1, &mut arg.is_escape));
                        }
                        _self.traverse_expr(depth + 1, &mut fn_decl.body);
                    })
                }
            }
        }
    }

    fn traverse_expr(&'b mut self, depth: Depth, expr: &'a mut Expr) {
        match expr {
            Expr::LValue(lvalue, _) => self.traverse_var(depth, lvalue),
            Expr::Nil(_) => (),
            Expr::Sequence(exprs, _) => {
                for expr in exprs {
                    self.traverse_expr(depth, expr);
                }
            }
            Expr::Int(_, _) => (),
            Expr::Str(_, _) => (),
            Expr::FuncCall(_, args, _) => {
                for expr in args {
                    self.traverse_expr(depth, expr);
                }
            }
            Expr::Op(_, lhs, rhs, _) => {
                self.traverse_expr(depth, lhs);
                self.traverse_expr(depth, rhs);
            }
            Expr::Neg(expr, _) => self.traverse_expr(depth, expr),
            Expr::RecordCreation(_, fields, _) => {
                for field in fields {
                    self.traverse_expr(depth, &mut field.expr)
                }
            }
            Expr::ArrayCreation { size, init, .. } => {
                self.traverse_expr(depth, size);
                self.traverse_expr(depth, init);
            }
            Expr::Assign(lvalue, expr, _) => {
                self.traverse_var(depth, lvalue);
                self.traverse_expr(depth, expr);
            }
            Expr::If {
                cond,
                then,
                els,
                pos: _,
            } => {
                self.traverse_expr(depth, cond);
                self.traverse_expr(depth, then);
                if let Some(els) = els {
                    self.traverse_expr(depth, els);
                }
            }
            Expr::While(cond, then, _) => {
                self.traverse_expr(depth, cond);
                self.traverse_expr(depth, then);
            }
            Expr::For {
                ref id,
                is_escape,
                from,
                to,
                then,
                pos: _,
            } => self.new_scope(|_self| {
                let sym = Symbol::from(id);
                _self.esc_env.enter(sym, (depth, is_escape));
                _self.traverse_expr(depth, from);
                _self.traverse_expr(depth, to);
                _self.traverse_expr(depth, then);
            }),
            Expr::Break(_) => (),
            Expr::Let(decls, exprs, _) => self.new_scope(|_self| {
                for decl in decls {
                    _self.traverse_decl(depth, decl);
                }

                for expr in exprs {
                    _self.traverse_expr(depth, expr);
                }
            }),
        }
    }
}

impl Default for EscapeFinder<'_> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Ident,
        parser::{
            self,
            ast::{Expr, FuncDecl, Program, TypeField},
        },
    };

    use super::*;

    #[test]
    fn test_find_escape() {
        let src = r#"
let 
    var output := "" /* true */

    function middle1(s: string) = /* false */
        output := concat(output, s)
    
    function middle2(n: int, m: int) = /* (false, true) */
        let function inner(n: int) = /* false */
            let 
                var hoge := 1
            in
                hoge := n;
                hoge := m;
            end
        in 
            inner(n)
        end
in
    middle1(output)
end
"#;

        let ast = parser::parse("test", src.as_bytes()).unwrap();

        let _ = match ast {
            Program::Expr(mut e) => {
                EscapeFinder::find_escape(&mut e);

                assert!(matches!(e,
                        Expr::Let(decls, _, _) if
                            matches!(&decls[..],
                                [Decl::Var(VarDecl(Ident(output), true, _, _, _)), Decl::Func(fn_decls)] if
                                    output == "output" &&
                                    matches!(&fn_decls[..],  [_, FuncDecl {
                                        name: Ident(middle2),
                                        params,
                                        body,
                                        ..
                                    }] if
                                        middle2 == "middle2" &&
                                        matches!(&params[..],
                                            [TypeField { id: Ident(n), is_escape: false, ..}, TypeField { id: Ident(m), is_escape: true, .. }] if
                                                n == "n" && m == "m" ) &&
                                                    matches!(body,
                                                        Expr::Let(decls, _, _) if
                                                            matches!(&decls[..],
                                                                [Decl::Func(fn_decls)] if
                                                                    matches!(&fn_decls[..], [FuncDecl { name: Ident(inner), params, .. }] if
                                                                        inner == "inner" &&
                                                                            matches!(&params[..], [TypeField { id: Ident(n), is_escape: false, .. }] if n == "n"))))))));
            }
            Program::Decls(_) => panic!(),
        };
    }
}
