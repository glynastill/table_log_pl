BEGIN;

-- drop old functions
DROP FUNCTION IF EXISTS table_log_pl (); -- ignore any error (but do not CASCADE)
DROP FUNCTION IF EXISTS table_log_pl_restore_table(varchar, varchar, char, char, char, timestamptz, char, int, int, varchar, varchar);

DROP FUNCTION IF EXISTS table_log_pl_init(int, text, text, text, text);
DROP FUNCTION IF EXISTS table_log_pl_init(int, text);
DROP FUNCTION IF EXISTS table_log_pl_init(int, text, text);
DROP FUNCTION IF EXISTS table_log_pl_init(int, text, text, text);

COMMIT;
