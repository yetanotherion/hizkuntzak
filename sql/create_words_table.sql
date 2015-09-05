DROP TABLE IF EXISTS words CASCADE;
DROP SEQUENCE IF EXISTS words_id;

CREATE TABLE words (
       id SERIAL primary key,
       word text NOT NULL,
       lang integer NOT NULL references languages(id)
);

CREATE SEQUENCE words_id;
