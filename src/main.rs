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

// This module will be generated upon compilation
mod generated;

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

fn get_timeout_from(matches: &clap::ArgMatches) -> u128 {
    matches.value_of("timeout")
        .map(str::to_string)
        .or(std::env::var("RHIZOME_TIMEOUT").ok())
        .and_then(|s| u128::from_str_radix(&s, 10).ok())
        .unwrap_or({
            println!("Using default timeout (10,000 milliseconds).");
            10_000
        })
}


#[actix_rt::main]
async fn main() -> std::io::Result<()> {
    use clap::{Arg, ArgGroup, SubCommand};

    let dbpath_arg = Arg::with_name("dbpath")
        .short("p")
        .long("db-path")
        .help("The path to the local SQLite database file.  [RHIZOME_DB_PATH]")
        .takes_value(true);
    let timeout_arg = Arg::with_name("timeout")
        .short("t")
        .long("timeout")
        .help("Maximum time allowed for hormone evaluation, in milliseconds. [RHIZOME_TIMEOUT]")
        .takes_value(true);
    let yes_arg = Arg::with_name("yes")
        .short("y")
        .long("yes")
        .help("Disable prompts and always choose the default option.");

    let matches = clap::App::new("Subterranean Articulation «Rhizome»")
        .version(format!(
            "rhizome-{} w/ hormone-{}",
            env!("CARGO_PKG_VERSION"),
            generated::HORMONE_VERSION,
        ).as_str())
        .arg(dbpath_arg.clone())
        .arg(Arg::with_name("listen")
             .short("l")
             .long("listen")
             .help("The address and port to listen to. [RHIZOME_LISTEN]")
             .takes_value(true))
        .arg(timeout_arg.clone())
        .arg(Arg::with_name("default")
             .long("default")
             .help("Fall back to the default option if any of the options are not specified."))
        .subcommand(SubCommand::with_name("init")
                    .about("Create a Rhizome database")
                    .arg(dbpath_arg.clone())
                    .arg(yes_arg.clone()))
        .subcommand(SubCommand::with_name("export")
                    .about("Render and save all content as HTML files")
                    .arg(dbpath_arg.clone())
                    .arg(Arg::with_name("output")
                         .short("o")
                         .long("output")
                         .help("The directory in which all exported files will be stored.")
                         .default_value("."))
                    .arg(Arg::with_name("raw")
                         .short("r")
                         .long("raw")
                         .help("Stores items as the unprocessed text files instead of the rendered HTML files."))
                    .arg(timeout_arg.clone()))
        .subcommand(SubCommand::with_name("import")
                    .about("Load raw .hmn files into a Rhizome database")
                    .arg(dbpath_arg.clone())
                    .arg(Arg::with_name("raw-input")
                         .short("r")
                         .long("raw-input")
                         .help("The directory in which the raw .hmn files to import are located.")
                         .takes_value(true))
                    .arg(Arg::with_name("input")
                         .short("i")
                         .long("input")
                         .help("The directory in which a subdirectory called 'raw' that contains the .hmn files to import are located.")
                         .takes_value(true))
                    .group(ArgGroup::with_name("input-dir")
                           .args(&["input", "raw-input"])
                           .required(true))
                    .arg(yes_arg.clone()))
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("init") {
        run_init(matches);
        return Ok(());
    } else if let Some(matches) = matches.subcommand_matches("export") {
        return run_export(matches);
    } else if let Some(matches) = matches.subcommand_matches("import") {
        return run_import(matches);
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

    let timeout = get_timeout_from(&matches);

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
            .service(get_raw_item)
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


fn run_export(matches: &clap::ArgMatches) -> std::io::Result<()> {
    use std::fs;
    use std::io::Write;
    use std::path::Path;

    use crate::models::*;
    use crate::schema::item::dsl::*;

    let dbpath = get_dbpath_from(matches, false);
    let output = matches.value_of("output").unwrap();
    let timeout = get_timeout_from(matches);
    let raw = matches.is_present("raw");

    println!(
        "Exporting {} content to '{}'.",
        if raw { "raw" } else { "processed" },
        &output
    );

    let manager = ConnectionManager::<SqliteConnection>::new(&dbpath);
    let conn = manager.connect()
        .expect(&format!("Unable to connect to {}.", &dbpath));

    let all_items = item
        .load::<Item>(&conn)
        .unwrap();
    let anchored_items = all_items.iter()
        .filter(|i| i.anchor)
        .collect();

    let lookup: Box<hormone::LookupFn> = Box::new(|iid| {
        item.find(&iid)
            .first::<Item>(&conn)
            .map(|i| i.text)
            .ok()
    });

    let out_dir = Path::new(output);
    fs::create_dir_all(out_dir)?;

    if !raw {
        fs::File::create(out_dir.join("style.css"))?
            .write(include_str!("../static/style.css").as_bytes())?;
        fs::File::create(out_dir.join("all.html"))?
            .write(render_list(
                &all_items.iter().collect(),
                "#[=-=]",
                ".",
                "html").as_bytes())?;
        fs::File::create(out_dir.join("anchorage.html"))?
            .write(render_list(&anchored_items, "#[^^^]", ".", "html").as_bytes())?;
    }

    for mut i in all_items {
        let back_path = iid_to_back_path(&i.id).display().to_string();
        let body_text = if raw {
            i.text
        } else {
            render_item(
                &lookup,
                &mut i,
                timeout,
                false,
                &back_path,
                "html"
            )
        };
        let path = out_dir.join(if raw { "raw" } else { "item" })
            .join(&i.id)
            .with_extension(if raw { "hmn" } else { "html" });
        fs::create_dir_all(path.parent().unwrap())?;
        let mut file = fs::File::create(&path)?;
        file.write(body_text.as_bytes())?;

        println!("Written:  '{}'", &path.display());

        if i.anchor && raw {
            let path = path.with_extension("anchor");
            let mut file = fs::File::create(&path)?;
            file.write(b"")?;
            println!("Created anchor file:  '{}'", &path.display());
        }
    }

    Ok(())
}

fn run_import(matches: &clap::ArgMatches) -> std::io::Result<()> {
    use walkdir::WalkDir;

    use std::fs;
    use std::path::{Path, PathBuf};

    use crate::models::*;
    use crate::schema::item::dsl::*;

    let dbpath = get_dbpath_from(matches, false);
    let input = matches.value_of("raw-input")
        .unwrap_or(matches.value_of("input").unwrap_or("."));
    let yes = matches.is_present("yes");
    let raw = matches.is_present("raw-input");

    if !yes {
        println!(
            "Import {}content at '{}' to new database at '{}'? [y/N]",
            if raw { "raw " } else { "" },
            Path::new(input).canonicalize()?.display(),
            &dbpath
        );
        let mut input = String::new();
        if !matches.is_present("yes") && (
            !std::io::stdin().read_line(&mut input).is_ok() ||
                !(input.starts_with("y") || input.starts_with("Y"))) {
            return Ok(());
        }
    } else {
        println!("Creating new database at '{}'.", &dbpath);
    }

    let input_path = Path::new(input).canonicalize()?;
    input_path.metadata()?;
    let raw_path = if raw {
        input_path.into()
    } else {
        input_path.join("raw")
    };
    raw_path.metadata()?;
    if !raw_path.is_dir() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Cannot found the 'raw' directory in the input path."
        ));
    }

    let mut items = Vec::new();
    let mut prelude = None;
    for entry in WalkDir::new(&raw_path)
        .into_iter()
        .filter_entry(
            |e| e.file_type().is_dir()
                ||
                (e.file_type().is_file()
                 &&
                 e.path().extension().and_then(std::ffi::OsStr::to_str)
                 ==
                 Some("hmn"))
        ) {
            let path = PathBuf::from(entry?.path());
            if path.is_dir() {
                continue;
            }

            let id_path = path.strip_prefix(&raw_path)
                .unwrap()
                .with_extension("");
            let i = Item {
                id: id_path.display().to_string(),
                text: fs::read_to_string(&path)?,
                anchor: path.with_extension("anchor").exists(),
            };

            if i.id == ".^" {
                prelude = Some(i);
            } else {
                items.push(i);
            }
        }

    let manager = ConnectionManager::<SqliteConnection>::new(&dbpath);
    embedded_migrations::run_with_output(
        &manager.connect().expect(&format!("Unable to connect to {}.", &dbpath)),
        &mut std::io::stdout()
    ).unwrap();

    let conn = manager.connect()
        .expect(&format!("Unable to connect to {}.", &dbpath));

    diesel::replace_into(item)
        .values(&prelude)
        .execute(&conn)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

    if let Some(i) = prelude {
        println!(
            "Imported: {}[{}]",
            if i.anchor { "^" } else { " " },
            i.id,
        );
    }

    diesel::insert_into(item)
        .values(&items)
        .execute(&conn)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

    for i in items.iter() {
        println!(
            "Imported: {}[{}]",
            if i.anchor { "^" } else { " " },
            i.id,
        );
    }

    println!("Done.");

    Ok(())
}


