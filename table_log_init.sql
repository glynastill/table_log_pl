SET search_path TO public;

CREATE OR REPLACE FUNCTION table_log_pl_init(level int, orig_schema text, orig_name text, log_schema text, log_name text) 
RETURNS void AS 
$BODY$
DECLARE
    do_log_user  int = 0;
    level_create text = E'''';
    orig_qq      text;
    log_qq       text;
BEGIN
    -- Quoted qualified names
    orig_qq := quote_ident(orig_schema)||'.'||quote_ident(orig_name);
    log_qq := quote_ident(log_schema)||'.'||quote_ident(log_name);

    IF level <> 3 THEN
        level_create := level_create
            ||', trigger_id BIGSERIAL NOT NULL PRIMARY KEY';
        IF level <> 4 THEN
            level_create := level_create
                ||', trigger_user VARCHAR(32) NOT NULL';
            do_log_user := 1;
            IF level <> 5 THEN
                RAISE EXCEPTION 
                    'table_log_pl_init: First arg has to be 3, 4 or 5.';
            END IF;
        END IF;
    END IF;
    
    EXECUTE 'CREATE TABLE '||log_qq
          ||'(LIKE '||orig_qq
          ||', trigger_mode VARCHAR(10) NOT NULL'
          ||', trigger_tuple VARCHAR(5) NOT NULL'
          ||', trigger_changed TIMESTAMPTZ NOT NULL'
          ||level_create
          ||')';
            
    EXECUTE 'CREATE TRIGGER "table_log_trigger_pl" AFTER UPDATE OR INSERT OR DELETE ON '
          ||orig_qq||' FOR EACH ROW EXECUTE PROCEDURE table_log_pl('
          ||quote_literal(log_name)||','
          ||do_log_user||','
          ||quote_literal(log_schema)||')';

    RETURN;
END;
$BODY$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION table_log_pl_init(level int, orig_name text) 
RETURNS void AS 

$BODY$
BEGIN
    PERFORM table_log_pl_init(level, orig_name, current_schema());
    RETURN;
END;
$BODY$
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION table_log_pl_init(level int, orig_name text, log_schema text) 
RETURNS void AS 
$BODY$
BEGIN
    PERFORM table_log_pl_init(level, current_schema(), orig_name, log_schema);
    RETURN;
END;
$BODY$
LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION table_log_pl_init(level int, orig_schema text, orig_name text, log_schema text) 
RETURNS void AS 
$BODY$
BEGIN
    PERFORM table_log_pl_init(level, orig_schema, orig_name, log_schema,
        CASE WHEN orig_schema=log_schema 
            THEN orig_name||'_log' ELSE orig_name END);
    RETURN;
END;
$BODY$
LANGUAGE plpgsql;
