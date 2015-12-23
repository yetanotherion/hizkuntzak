ALTER TABLE translations
ADD correction_state integer,
ADD correction_link integer;

UPDATE translations
SET correction_state = 0,
    correction_link = -1;

ALTER TABLE translations
ALTER correction_state SET NOT NULL,
ALTER correction_link SET NOT NULL;
