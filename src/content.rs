use crate::ast::content_tree::*;
use std::collections::HashMap;

pub type Scope<T> = HashMap<String, T>;
pub type Context<T> = Vec<Scope<T>>; // Context is a stack of scopes

pub trait ContextMethods<T, U> {
    fn update_var(&mut self, key: &str, val: &T) -> U;
    fn drop_current_scope(&mut self);
    fn get_val(&mut self, key: &str) -> U;
    fn insert_to_current_scope(&mut self, key: &str, val: &T);
    fn new_scope(&mut self);
}

impl ContextMethods<Content, Content> for Context<Content> {
    fn update_var(&mut self, key: &str, val: &Content) -> Content {
        for scope in self.iter_mut().rev() {
            match scope.get(key) {
                Some(_) => {
                    scope.insert(key.to_string(), val.clone());
                    return val.clone();
                }
                None => continue,
            }
        }
        Content::Null
    }

    fn get_val(&mut self, key: &str) -> Content {
        let mut val_res: Content = Content::Null;

        for scope in self.iter().rev() {
            match scope.get(key) {
                Some(content) => {
                    val_res = content.clone();
                    break;
                }
                None => continue,
            };
        }

        val_res
    }

    fn insert_to_current_scope(&mut self, key: &str, val: &Content) {
        let scope_opt = self.last_mut();
        match scope_opt {
            Some(scope) => scope.insert(key.to_string(), val.clone()),
            None => panic!("There are no scopes in the context."),
        };
    }

    fn new_scope(&mut self) {
        let scope: Scope<Content> = HashMap::new();
        self.push(scope);
    }

    fn drop_current_scope(&mut self) {
        self.pop();
    }
}
