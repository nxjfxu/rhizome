CREATE TABLE item (
    id TEXT PRIMARY KEY NOT NULL,
    anchor BOOLEAN NOT NULL DEFAULT false,
    text TEXT NOT NULL
);

INSERT INTO item (id, anchor, text) VALUES (".^", true, "#!");

