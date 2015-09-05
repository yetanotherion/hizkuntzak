DROP TABLE IF EXISTS users CASCADE;
DROP SEQUENCE IF EXISTS users_id;

CREATE TABLE users (
       id SERIAL primary key,
       username text NOT NULL,
       password text NOT NULL);

CREATE SEQUENCE users_id;
