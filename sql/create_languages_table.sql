DROP TABLE IF EXISTS languages CASCADE;
CREATE TABLE languages (
       id integer primary key,
       lang text NOT NULL
);


INSERT INTO languages VALUES (0, 'eus');
INSERT INTO languages VALUES (1, 'en');
INSERT INTO languages VALUES (2, 'fr');
INSERT INTO languages VALUES (3, 'ru');
INSERT INTO languages VALUES (4, 'es');
