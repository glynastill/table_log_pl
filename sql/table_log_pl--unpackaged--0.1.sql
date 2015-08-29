\echo Use "CREATE EXTENSION table_log_pl FROM unpackaged" to load this file. \quit

ALTER EXTENSION table_log_pl ADD FUNCTION table_log_pl();
ALTER EXTENSION table_log_pl ADD FUNCTION table_log_pl_restore_table(varchar, varchar, char, char, char, timestamptz, char, int, int, varchar, varchar);
ALTER EXTENSION table_log_pl ADD FUNCTION table_log_pl_init(int, text, text, text, text);
ALTER EXTENSION table_log_pl ADD FUNCTION table_log_pl_init(int, text);
ALTER EXTENSION table_log_pl ADD FUNCTION table_log_pl_init(int, text, text);
ALTER EXTENSION table_log_pl ADD FUNCTION table_log_pl_init(int, text, text, text);
