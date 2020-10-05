use serde::Serialize;

use tera::Context;

use crate::schema::item;


#[derive(AsChangeset, Debug, Insertable, Queryable, Serialize)]
#[table_name = "item"]
pub struct Item {
    pub id: String,
    pub anchor: bool,
    pub text: String,
}


impl Item {
    pub fn to_context(&self) -> Context {
        let mut ctx = Context::new();
        ctx.insert("id", &self.id);
        ctx.insert("anchor", &self.anchor);
        ctx.insert("text", &self.text);
        ctx
    }
}
