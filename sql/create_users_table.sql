DROP TABLE IF EXISTS users CASCADE;
DROP SEQUENCE IF EXISTS users_id;

CREATE TABLE users (
       id SERIAL primary key,
       username text NOT NULL,
       password text NOT NULL,
       preferred_lang_src integer NOT NULL references languages(id),
       preferred_lang_dst integer NOT NULL references languages(id),
       last_visit_date date NOT NULL);

CREATE SEQUENCE users_id;
