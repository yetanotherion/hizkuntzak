ALTER TABLE users
ADD preferred_lang_src integer;

ALTER TABLE users
RENAME preferred_lang TO preferred_lang_dst;

UPDATE users
SET preferred_lang_src = 0;

ALTER TABLE users
ADD CONSTRAINT preferred_lang_src
FOREIGN KEY (preferred_lang_src)
REFERENCES languages(id);
