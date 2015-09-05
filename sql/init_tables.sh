#!/bin/bash
psql -d hizkuntzak -f create_languages_table.sql
psql -d hizkuntzak -f create_words_table.sql
psql -d hizkuntzak -f create_users_table.sql
psql -d hizkuntzak -f create_translations_table.sql
