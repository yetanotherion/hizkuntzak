DROP TABLE IF EXISTS translations CASCADE;
DROP SEQUENCE IF EXISTS translations_id;

CREATE TABLE translations (
       id SERIAL primary key,
       l_word integer NOT NULL references words(id),
       r_word integer NOT NULL references words(id),
       user_id integer NOT NULL references users(id),
       description TEXT NOT NULL,
       correction_state integer NOT NULL,
       correction_link integer NOT NULL);

CREATE SEQUENCE translations_id;
