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
    HttpServer,
};

use diesel::prelude::*;
use diesel::r2d2::*;

use lru::LruCache;


use std::sync::{Arc, RwLock};

pub mod schema;
pub mod models;

mod handler;

use self::handler::*;



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


