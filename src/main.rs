#[macro_use]
extern crate actix_web;
#[macro_use]
extern crate diesel;
#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate diesel_migrations;


use actix_web::{
    middleware,
    web,

    App,
    HttpResponse,
    HttpServer,
    Result,
};

use diesel::prelude::*;
use diesel::r2d2::*;

use lru::LruCache;

use serde::{Deserialize, Serialize};

use tera::{Context, Tera};


use std::sync::{Arc, RwLock};
use std::time::Instant;

pub mod schema;
pub mod models;

use self::models::*;
use self::schema::item::dsl::*;


use hormone::*;


lazy_static! {
    static ref TEMPLATES: Tera = {
        let mut tera = Tera::default();
        tera.add_raw_templates(vec![
            ("topnav.html", include_str!("../template/topnav.html")),
            ("item.html", include_str!("../template/item.html")),
            ("edit.html", include_str!("../template/edit.html")),
            ("list.html", include_str!("../template/list.html")),
        ]).unwrap();
        tera.autoescape_on(vec![".html"]);
        tera
    };
}


fn is_valid_id(iid: &str) -> bool {
    iid.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '/')
}


fn new_inner(iid: Option<&str>) -> Result<HttpResponse> {
    let mut context = Context::new();
    context.insert("id", iid.unwrap_or(""));
    context.insert("anchor", &false);
    context.insert("text", &"");
    context.insert("new", &true);
    context.insert("row_count", &10);
    Ok(web::HttpResponse::Ok()
       .body(TEMPLATES.render("edit.html", &context).unwrap())
    )
}


#[get("/anchorage")]
async fn anchorage(
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
) -> Result<HttpResponse> {
    let anchored_items = item
        .filter(anchor.eq(true))
        .load::<Item>(&db.get().unwrap())
        .unwrap();
    let mut context = Context::new();
    context.insert("items", &anchored_items);
    context.insert("title", "#[^^^]");
    Ok(web::HttpResponse::Ok()
       .content_type("text/html; charset=UTF-8")
       .body(TEMPLATES.render("list.html", &context)
             .unwrap())
    )
}

#[get("/all")]
async fn all(
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
) -> Result<HttpResponse> {
    let all_items = item
        .load::<Item>(&db.get().unwrap())
        .unwrap();
    let mut context = Context::new();
    context.insert("items", &all_items);
    context.insert("title", "#[=-=]");
    Ok(web::HttpResponse::Ok()
       .content_type("text/html; charset=UTF-8")
       .body(TEMPLATES.render("list.html", &context)
             .unwrap())
    )
}

#[get("/item/{id:.+}")]
async fn get_item(
    iid: web::Path<String>,
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
    timeout: web::Data<u128>,
) -> Result<HttpResponse> {
    let iid = iid.into_inner();
    let query_result = item.find(&iid)
        .first::<Item>(&db.get().unwrap());

    match query_result {
        Ok(mut i) => {
            let f: Box<LookupFn> = Box::new(|iid| {
                item.find(iid)
                    .first::<Item>(&db.get().unwrap())
                    .map(|i| i.text)
                    .ok()
            });

            let begin = Instant::now();
            let evaluation = evaluate_timeout(
                &iid,
                &f,
                *timeout.into_inner(),
                true,
                &i.text
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

            Ok(web::HttpResponse::Ok()
               .body(TEMPLATES.render("item.html", &ctx).unwrap())
            )
        },

        _ => new_inner(Some(&iid)),
    }
}

#[get("/edit/{id:.+}")]
async fn edit_item(
    iid: web::Path<String>,
    db: web::Data<Arc<Pool<ConnectionManager<SqliteConnection>>>>,
) -> Result<HttpResponse> {
    let query_result = item.find(iid.into_inner())
        .first::<Item>(&db.get().unwrap());

    match query_result {
        Ok(i) => {
            let mut context = i.to_context();
            context.insert("new", &false);
            context.insert(
                "row_count",
                &(i.text.lines().count() + 10)
            );

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
struct ItemParams {
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
async fn post_item(
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
async fn new_item() -> Result<HttpResponse> {
    new_inner(None)
}

#[derive(Serialize, Deserialize)]
struct NewItemParams {
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
async fn post_new_item(
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
        return Ok(web::HttpResponse::BadRequest()
                  .body(TEMPLATES.render("edit.html", &context).unwrap())
        )
    }

    let result = diesel::insert_into(schema::item::table)
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

#[get("/style.css")]
async fn style_css() -> Result<HttpResponse> {
    Ok(web::HttpResponse::Ok()
       .content_type("text/css")
       .body(include_str!("../static/style.css"))
    )
}

async fn not_found() -> Result<HttpResponse> {
    Ok(web::HttpResponse::NotFound()
       .body("not found.")
    )
}


fn get_dbpath_from(matches: &clap::ArgMatches, default: bool) -> String {
    matches.value_of("dbpath")
        .map(String::from)
        .or(std::env::var("RHIZOME_DB_PATH").ok())
        .or(if default {
            println!("Using default database path.");
            Some(String::from("data.db"))
        } else {
            None
        })
        .expect("Database path is not given.  Pleaee specify a path by either 1) setting the RHIZOME_DB_PATH environment variable 2) supplying the --db-path (-p) argument 3) use the --default flag to use the default option.")
}


#[actix_rt::main]
async fn main() -> std::io::Result<()> {
    use clap::{Arg, SubCommand};

    let matches = clap::App::new("Subterranean Articulation «Rhizome»")
        .version(env!("CARGO_PKG_VERSION"))
        .arg(Arg::with_name("dbpath")
             .short("p")
             .long("db-path")
             .help("The path to the local SQLite database file.  [RHIZOME_DB_PATH]")
             .takes_value(true))
        .arg(Arg::with_name("listen")
             .short("l")
             .long("listen")
             .help("The address and port to listen to. [RHIZOME_LISTEN]")
             .takes_value(true))
        .arg(Arg::with_name("timeout")
             .short("t")
             .long("timeout")
             .help("Maximum time allowed for hormone evaluation, in milliseconds. [RHIZOME_TIMEOUT]")
             .takes_value(true))
        .arg(Arg::with_name("default")
             .long("default")
             .help("Fall back to the default option if any of the options are not specified."))
        .subcommand(SubCommand::with_name("init")
                    .about("Create a Rhizome database")
                    .arg(Arg::with_name("dbpath")
                         .short("p")
                         .long("db-path")
                         .help("The path to the local SQLite database file.  [RHIZOME_DB_PATH]")
                         .takes_value(true))
                    .arg(Arg::with_name("yes")
                         .short("y")
                         .long("yes")
                         .help("Disable prompts and always choose the default option.")))
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("init") {
        run_init(matches);
        return Ok(());
    }

    let default = matches.is_present("default");

    let dbpath = get_dbpath_from(&matches, default);

    print!(
        "Connecting to textual database at: {} ...",
        dbpath
    );

    let manager = ConnectionManager::<SqliteConnection>::new(dbpath);

    println!("Ok.");

    let pool = Arc::new(Pool::new(manager).unwrap());
    let cache = Arc::new(RwLock::new(LruCache::<String, String>::new(100)));

    let listen = matches.value_of("listen")
        .map(String::from)
        .or(std::env::var("RHIZOME_LISTEN").ok())
        .or(if default {
            println!("Using default listen address.");
            Some(String::from("127.0.0.1:8001"))
        } else {
            None
        })
        .expect("The address to listen is not given.  Pleaee specify an address by either 1) setting the RHIZOME_LISTEN environment variable 2) supplying the --listen (-l) argument 3) use the --default flag to use the default option.");

    println!(
        "Listening: {}.",
        listen,
    );

    let timeout = matches.value_of("timeout")
        .map(str::to_string)
        .or(std::env::var("RHIZOME_TIMEOUT").ok())
        .and_then(|s| u128::from_str_radix(&s, 10).ok())
        .unwrap_or({
            println!("Using default timeout (10,000 milliseconds).");
            10_000
        });

    HttpServer::new(move || {
        App::new()
            .app_data(web::FormConfig::default().limit(5_000_000))
            .data(pool.clone())
            .data(cache.clone())
            .data(timeout)
            .wrap(middleware::DefaultHeaders::new()
                  .header("Content-Type", "text/html"))
            .service(anchorage)
            .service(all)
            .service(get_item)
            .service(edit_item)
            .service(post_item)
            .service(new_item)
            .service(post_new_item)
            .service(style_css)
            .default_service(web::resource("")
                             .route(web::get().to(not_found)))
    })
    .bind(listen)?
    .run()
    .await
}



embed_migrations!();

fn run_init(matches: &clap::ArgMatches) {
    let dbpath = get_dbpath_from(matches, false);
    let yes = matches.is_present("yes");

    if !yes {
        println!("Create new database at '{}'? [y/N]", &dbpath);
        let mut input = String::new();
        if !matches.is_present("yes") && (
            !std::io::stdin().read_line(&mut input).is_ok() ||
                !(input.starts_with("y") || input.starts_with("Y"))) {
            return;
        }
    } else {
        println!("Creating new database at '{}'.", &dbpath);
    }

    let manager = ConnectionManager::<SqliteConnection>::new(&dbpath);

    embedded_migrations::run_with_output(
        &manager.connect().expect(&format!("Unable to connect to {}.", &dbpath)),
        &mut std::io::stdout()
    ).unwrap();
    println!("Done.");
}


