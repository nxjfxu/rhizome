# Subterranean Articulation «Rhizome»

Rhizome is a text-weaving tool with a web interface.
It consists of a minimalist storage of text fragments,
and an interpreter for *Hormone*,
a scripting language for text processing.

[Document](https://nxjfxu.github.io/rhizome-guide/item/guide.html)

Alternatively, you can find the guide
[in this repository](https://github.com/nxjfxu/rhizome-guide).
You can import the guide following the instructions in the rhizome-guide repository.


## Running Rhizome

### Install

Make sure you have the following requirements ready:

+ [Rust](https://rust-lang.org)
+ [SQLite 3](https://sqlite.org)

If you can run `cargo` and `sqlite3` on the command line, then you should be fine.

Run:
```
cargo install --git https://github.com/nxjfxu/rhizome --branch main
```
to install the latest stable version of Rhizome.

After Cargo finishes installing, you should be able to run the `rhizome` on the command line.


### Setup

Rhizome stores textual data in an SQLite 3 database.
Before running Rhizome, you need to create the database file.

Run:
```
rhizome init -p <path-to-database>
```
to create a fresh database file at `<path-to-database>`.

### Run

Run:
```
rhizome -p <path-to-database> -l 127.0.0.1:8001
```
to start a Rhizome server listening to `127.0.0.1:8001`
serving data stored in the database file at `<path-to-database>`.

You can now visit http://127.0.0.1:8001/anchorage to enjoy the
nutrients stored in your local rhizome.

For more options, run: `rhizome -h`:

