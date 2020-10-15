use actix_web::{
    web,

    HttpResponse,
    Result,
};

use diesel::prelude::*;
use diesel::r2d2::*;

use lru::LruCache;

use serde::{Deserialize, Serialize};

use tera::{Context, Tera};

use std::sync::{Arc, RwLock};
use std::time::Instant;


use crate::models::*;
use crate::schema::item::dsl::*;


use hormone::*;


lazy_static! {
    static ref TEMPLATES: Tera = {
        let mut tera = Tera::default();
        tera.add_raw_templates(vec![
            ("head.html", include_str!("../template/head.html")),
            ("topnav.html", include_str!("../template/topnav.html")),
            ("item.html", include_str!("../template/item.html")),
            ("edit.html", include_str!("../template/edit.html")),
            ("list.html", include_str!("../template/list.html")),
        ]).unwrap();
        tera.autoescape_on(vec![".html"]);
        tera
    };
}


pub fn iid_to_back_path(iid: &str) -> std::path::PathBuf {
    std::path::Path::new(iid).components()
        .fold(std::path::PathBuf::from("."), |pb, _| pb.join(".."))
}


fn is_valid_id(iid: &str) -> bool {
    iid.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '/')
}


fn new_inner(iid: Option<&str>, back_path: &str) -> Result<HttpResponse> {
    let mut context = Context::new();
    context.insert("id", iid.unwrap_or(""));
    context.insert("anchor", &false);
    context.insert("text", &"");
    context.insert("new", &true);
    context.insert("row_count", &10);
    context.insert("back_path", back_path);
    context.insert("edit", &true);
    Ok(web::HttpResponse::Ok()
       .body(TEMPLATES.render("edit.html", &context).unwrap())
    )
}


pub fn render_list(
    items: &Vec<&Item>,
    title: &str,
    back_path: &str,
    page_extension: &str,
) -> String {
    let page_extension = if page_extension == "" {
        page_extension.to_string()
    } else {
        format!(".{}", page_extension)
    };
    let mut context = Context::new();
    context.insert("items", &items);
    context.insert("title", title);
    context.insert("back_path", back_path);
    context.insert("edit", &true);
    context.insert("page_extension", &page_extension);
    TEMPLATES.render("list.html", &context).unwrap()
}

pub fn render_text(
    lookup: &LookupFn,
    i: &mut Item,
    timeout: u128,
    page_extension: &str,
) -> String {
    let evaluation = evaluate_timeout(
        &i.id,
        &lookup,
        timeout,
        true,
        &i.text,
        page_extension,
    );
    match &evaluation {
        Ok(e) => e.expr.to_string(),
        Err(e) => format!("Error: {}", e),
    }
}

pub fn render_item(
    lookup: &LookupFn,
    i: &mut Item,
    timeout: u128,
    edit: bool,
    back_path: &str,
    page_extension: &str,
) -> String {
    let begin = Instant::now();
    let evaluation = evaluate_timeout(
        &i.id,
        &lookup,
        timeout,
        true,
        &i.text,
        page_extension,
    );
    i.text = match &evaluation {
        Ok(e) => e.expr.to_string(),
        Err(e) => format!("<span class=\"error\">{}</span>", e),
    };
    let cycles = match &evaluation {
        Ok(e) => e.cycles,
        Err(_) => 0,
    };

    let time_spent = begin.elapsed().as_nanos();
    let time_spent_string = format!(
        "{}.{:02}ms",
        time_spent / 1_000_000,
        (time_spent % 1_000_000) / 10_000,
    );
    let mut ctx = i.to_context();
    ctx.insert("time_spent", &time_spent_string);
    ctx.insert("cycles", &cycles);
    ctx.insert("edit", &edit);
    ctx.insert("back_path", back_path);

    let page_extension = if page_extension == "" {
        page_extension.to_string()
    } else {
        format!(".{}", page_extension)
    };
    ctx.insert("page_extension", &page_extension);

    TEMPLATES.render("item.html", &ctx).unwrap()
}


#[get("/anchorage")]
pub async fn anchorage(
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
) -> Result<HttpResponse> {
    let anchored_items = item
        .filter(anchor.eq(true))
        .load::<Item>(&db.get().unwrap())
        .unwrap();
    Ok(web::HttpResponse::Ok()
       .content_type("text/html; charset=UTF-8")
       .body(render_list(
           &anchored_items.iter().collect(),
           "#[^^^]",
           ".",
           ""
       ))
    )
}

#[get("/all")]
pub async fn all(
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
) -> Result<HttpResponse> {
    let all_items = item
        .load::<Item>(&db.get().unwrap())
        .unwrap();
    Ok(web::HttpResponse::Ok()
       .content_type("text/html; charset=UTF-8")
       .body(render_list(
           &all_items.iter().collect(),
           "#[=-=]",
           ".",
           ""
       ))
    )
}

#[get("/item/{id:.+}")]
pub async fn get_item(
    iid: web::Path<String>,
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
    timeout: web::Data<u128>,
) -> Result<HttpResponse> {
    let iid = iid.into_inner();
    let query_result = item.find(&iid)
        .first::<Item>(&db.get().unwrap());
    let back_path = iid_to_back_path(&iid).display().to_string();

    match query_result {
        Ok(mut i) => {
            let lookup: Box<LookupFn> = Box::new(|iid| {
                item.find(&iid)
                    .first::<Item>(&db.get().unwrap())
                    .map(|i| i.text)
                    .ok()
            });

            let body_text = render_item(
                &lookup,
                &mut i,
                *timeout.into_inner(),
                true,
                &back_path,
                ""
            );
            Ok(web::HttpResponse::Ok().body(body_text))
        },

        _ => new_inner(Some(&iid), &back_path),
    }
}

#[get("/raw/{id:.+}")]
pub async fn get_raw_item(
    iid: web::Path<String>,
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
) -> Result<HttpResponse> {
    let iid = iid.into_inner();
    let query_result = item.find(&iid)
        .first::<Item>(&db.get().unwrap());

    match query_result {
        Ok(i) => Ok(web::HttpResponse::Ok()
                    .content_type("text/plain; charset=utf-8")
                    .body(i.text)),

        _ => Ok(web::HttpResponse::NotFound()
                .body("not found")),
    }
}

#[get("/edit/{id:.+}")]
pub async fn edit_item(
    iid: web::Path<String>,
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
) -> Result<HttpResponse> {
    let iid = iid.into_inner();
    let query_result = item.find(&iid)
        .first::<Item>(&db.get().unwrap());

    match query_result {
        Ok(i) => {
            let mut context = i.to_context();
            context.insert("new", &false);
            context.insert(
                "row_count",
                &(i.text.lines().count() + 10)
            );
            context.insert("back_path", &iid_to_back_path(&iid));
            context.insert("edit", &true);

            Ok(web::HttpResponse::Ok()
               .body(TEMPLATES.render("edit.html", &context).unwrap())
            )
        },

        _ => Ok(web::HttpResponse::NotFound()
                .body("not found")
        ),
    }
}

#[derive(Serialize, Deserialize)]
pub struct ItemParams {
    text: String,
    anchor: Option<String>,
}

impl ItemParams {
    fn to_item(&self, iid: &String) -> Item {
        Item {
            id: iid.clone(),
            anchor: self.anchor == Some(String::from("on")),
            text: self.text.clone(),
        }
    }
}

#[post("/item/{id:.+}")]
pub async fn post_item(
    iid: web::Path<String>,
    params: web::Form<ItemParams>,
    cache: web::Data<Arc<RwLock<LruCache<String, String>>>>,
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
) -> Result<HttpResponse> {
    let iid = iid.into_inner();
    let result = diesel::update(item.find(&iid))
        .set(&params.to_item(&iid))
        .execute(&db.get().unwrap());

    match result {
        Ok(_) => {
            if let Ok(mut cache) = cache.write() {
                cache.pop(&iid);
            }

            Ok(web::HttpResponse::SeeOther()
               .header("Location", format!("/item/{}", &iid))
               .finish())
        },

        Err(e) =>
            Ok(web::HttpResponse::InternalServerError()
               .body(format!("Error: {:?}", e))),
    }
}

#[get("/new")]
pub async fn new_item() -> Result<HttpResponse> {
    new_inner(None, ".")
}

#[derive(Serialize, Deserialize)]
pub struct NewItemParams {
    id: String,
    text: String,
    anchor: Option<String>,
}

impl NewItemParams {
    fn to_item(&self) -> Item {
        Item {
            id: self.id.clone(),
            anchor: self.anchor == Some(String::from("on")),
            text: self.text.clone(),
        }
    }
}

#[post("/new")]
pub async fn post_new_item(
    params: web::Form<NewItemParams>,
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
) -> Result<HttpResponse> {
    if !is_valid_id(&params.id) {
        let mut context = Context::new();
        context.insert("id", &params.id);
        context.insert("anchor", &params.anchor);
        context.insert("text", &params.text);
        context.insert("new", &true);
        context.insert("error", &"#!");
        context.insert("back_path", ".");
        context.insert("edit", &true);
        return Ok(web::HttpResponse::BadRequest()
                  .body(TEMPLATES.render("edit.html", &context).unwrap())
        )
    }

    let result = diesel::insert_into(crate::schema::item::table)
        .values(&params.to_item())
        .execute(&db.get().unwrap());

    match result {
        Ok(_) => Ok(web::HttpResponse::SeeOther()
                    .header("Location", format!("/item/{}", &params.id))
                    .finish()),

        Err(e) =>
            Ok(web::HttpResponse::InternalServerError()
               .body(format!("Error: {:?}", e))),
    }
}

#[get("/default-style.css")]
pub async fn default_style_css() -> Result<HttpResponse> {
    Ok(web::HttpResponse::Ok()
       .content_type("text/css")
       .body(include_str!("../static/style.css"))
    )
}

#[get("/style.css")]
pub async fn style_css(
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
    timeout: web::Data<u128>,
) -> Result<HttpResponse> {
    let iid = ".*";
    let query_result = item.find(&iid)
        .first::<Item>(&db.get().unwrap());

    match query_result {
        Ok(mut i) => {
            let lookup: Box<LookupFn> = Box::new(|iid| {
                item.find(&iid)
                    .first::<Item>(&db.get().unwrap())
                    .map(|i| i.text)
                    .ok()
            });

            let body_text = render_text(
                &lookup,
                &mut i,
                *timeout.into_inner(),
                ""
            );
            Ok(web::HttpResponse::Ok()
               .content_type("text/css")
               .body(body_text)
            )
        },

        _ => Ok(web::HttpResponse::Ok()
                .content_type("text/css")
                .body("")
        ),
    }
}

pub async fn not_found() -> Result<HttpResponse> {
    Ok(web::HttpResponse::NotFound()
       .body("not found.")
    )
}
