-- tachikoma up
CREATE TABLE IF NOT EXISTS account (
    id SERIAL PRIMARY KEY,
    uuid TEXT UNIQUE,
    name TEXT UNIQUE,
    password TEXT,
    image TEXT,
    createdAt TIMESTAMP WITH TIME ZONE,
    updatedAt TIMESTAMP WITH TIME ZONE
);
CREATE TABLE IF NOT EXISTS notebook (
    id SERIAL PRIMARY KEY,
    uuid TEXT,
    notebookId INTEGER,
    accountId INTEGER,
    name TEXT,
    createdAt TIMESTAMP WITH TIME ZONE,
    updatedAt TIMESTAMP WITH TIME ZONE
);

CREATE TABLE IF NOT EXISTS page (
    id SERIAL PRIMARY KEY,
    uuid TEXT UNIQUE,
    accountId INTEGER,
    notebookId INTEGER,
    title TEXT,
    content TEXT,
    createdAt TIMESTAMP WITH TIME ZONE,
    updatedAt TIMESTAMP WITH TIME ZONE
);

CREATE TABLE IF NOT EXISTS file (
    id SERIAL PRIMARY KEY,
    owner INTEGER,
    createAt timestamp with time zone,
    updateAt timestamp with time zone,
    name TEXT
);
-- tachikoma down
DROP TABLE IF EXISTS account;
DROP TABLE IF EXISTS notebook;
DROP TABLE IF EXISTS page;
DROP TABLE IF EXISTS file;
