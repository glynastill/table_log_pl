\echo Use "ALTER EXTENSION table_log_pl UPDATE TO '0.2'" to load this file. \quit

CREATE OR REPLACE FUNCTION table_log_pl() RETURNS TRIGGER AS
$BODY$
DECLARE
    v_tabname text;
    v_loguser boolean := false;
    v_nspname text;
    v_num_col int;
    v_num_col_log int;
    v_col_trig text := '';
    v_val_trig text := '';
    v_cols text := '';
    v_sql text;
    v_col_cache text;
    v_enable_cache boolean := true;
    v_enable_prepare boolean := false;
    v_tmp text;
    v_i int;

    v_cols_typ text;
    v_cols_nam text[];
    v_vals_old text = '';
    v_vals_new text = '';
BEGIN
    -- Notes:
    --     - The trigger_id comes off sequence, this function is oblivious
    --     - 3 columns means don't log trigger_user or trigger_id
    --     - 4 columns means don't log trigger_user
    --     - 5 columns means log both 
    --     - To use the column data caching on server versions prior to 
    --       9.6 add custom var "table_log.column_cache = ''" to postgresql.conf

    IF (TG_NARGS > 2) THEN
        v_nspname := TG_ARGV[2];
    ELSE
        v_nspname := TG_TABLE_SCHEMA;
    END IF;
        
    IF (TG_NARGS > 1 AND TG_ARGV[1]::int = 1) THEN
        v_loguser := true;
    END IF;
    
    IF (TG_NARGS > 0) THEN
        v_tabname := TG_ARGV[0];
    ELSE
        v_tabname := TG_TABLE_NAME || '_log';
    END IF;

    -- Retrieve custom variable used as a poor mans cache for multirow statements
    IF (v_enable_cache) THEN
        IF (current_setting('server_version_num')::int >= 90600) THEN
            v_col_cache := current_setting('table_log.column_cache', true);
        ELSE
            v_col_cache := current_setting('table_log.column_cache');
        END IF;
    END IF;
    
    -- If column caching is enabled and previous call in this transaction 
    -- was for the same relation we can retrieve column detail.
    IF (v_enable_cache AND left(v_col_cache, length(TG_RELID::text)+1) = (TG_RELID::text || ':')) THEN
        v_cols := right(v_col_cache, (length(TG_RELID::text)+1)*-1);
        v_cols_nam := ('{' || right(v_col_cache, (length(TG_RELID::text)+1)*-1) || '}')::text[];
    ELSE -- Otherwise fetch the column detail
        IF (TG_WHEN != 'AFTER') THEN
            RAISE EXCEPTION 'table_log: must be fired after event';
        END IF;
        IF (TG_LEVEL = 'STATEMENT') THEN
            RAISE EXCEPTION 'table_log: can''t process STATEMENT events';
        END IF;
    
        SELECT count(*), string_agg(quote_ident(attname),','), string_agg(format_type(atttypid, atttypmod),','), array_agg(quote_ident(attname))
        INTO STRICT v_num_col, v_cols, v_cols_typ, v_cols_nam
        FROM pg_catalog.pg_attribute
        WHERE attrelid = TG_RELID
        AND attnum > 0
        AND NOT attisdropped;
        
        IF (v_num_col < 1) THEN
            RAISE EXCEPTION 'table_log: number of columns in table is < 1, can this happen?';
        END IF;
            
        SELECT count(*) INTO STRICT v_num_col_log
        FROM pg_catalog.pg_attribute
        WHERE attrelid = (v_nspname || '.' || v_tabname)::regclass
        AND attnum > 0
        AND NOT attisdropped;
        
        IF (v_num_col_log < 1) THEN
            RAISE EXCEPTION 'could not get number columns in relation %.%', v_nspname, v_tabname;
        END IF;

        -- This is the way the original checks column count regardless of trigger_id is presence
        IF (v_num_col_log != (v_num_col + 3 + v_loguser::int)) AND (v_num_col_log != (v_num_col + 4 + v_loguser::int)) THEN
            RAISE EXCEPTION 'number colums in relation %.%(%) does not match columns in %.%(%)', TG_TABLE_SCHEMA, TG_TABLE_NAME, v_num_col, v_nspname, v_tabname, v_num_col_log;
        END IF;
        
        -- Set custom variable for use as a poor mans cache for multirow statements
        IF (v_enable_cache) THEN
            v_col_cache := (TG_RELID::text || ':' || v_cols);
            PERFORM set_config('table_log.column_cache', v_col_cache, true);
        END IF;
        
        -- Create a prepared statement for the current table, deallocating
        -- any old statements we may have prepared.
        IF (v_enable_prepare) THEN
            FOR v_tmp IN (SELECT name FROM pg_catalog.pg_prepared_statements WHERE name ~ '^table_log_pl_') LOOP
                EXECUTE format('DEALLOCATE %I', v_tmp);
            END LOOP;
            
            SELECT '$' || string_agg(a::text, ', $') INTO v_col_trig FROM generate_series(1,v_num_col+3+v_loguser::int) a;
            
            IF (v_loguser) THEN
                v_sql := format('PREPARE table_log_pl_%s(%s, text, text, timestamptz, text) AS INSERT INTO %I.%I (%s, "trigger_user", "trigger_mode", "trigger_changed", "trigger_tuple") VALUES (%s)', TG_RELID, v_cols_typ, v_nspname, v_tabname, v_cols, v_col_trig);
            ELSE
                v_sql := format('PREPARE table_log_pl_%s(%s, text, timestamptz, text) AS INSERT INTO %I.%I (%s, "trigger_mode", "trigger_changed", "trigger_tuple") VALUES (%s)', TG_RELID, v_cols_typ, v_nspname, v_tabname, v_cols, v_col_trig);
            END IF;
            EXECUTE v_sql;
        END IF;        
    END IF;
    
    -- If prepared statement method is enabled, construct strings for
    -- variable parameters and execute.
    IF (v_enable_prepare) THEN 
        FOR v_i IN 1..array_upper(v_cols_nam, 1) LOOP
            IF (TG_OP != 'INSERT') THEN
                EXECUTE 'SELECT ($1).' || v_cols_nam[v_i] || '::text' INTO v_tmp USING OLD;
                v_vals_old :=  v_vals_old || quote_nullable(v_tmp) || ',';
            END IF;
            IF (TG_OP != 'DELETE') THEN
                EXECUTE 'SELECT ($1).' || v_cols_nam[v_i] || '::text' INTO v_tmp USING NEW;
                v_vals_new :=  v_vals_new || quote_nullable(v_tmp) || ',';
            END IF;
        END LOOP;
        
        IF (v_loguser) THEN
            v_vals_new :=  v_vals_new || quote_literal(session_user) || ',';
            v_vals_old :=  v_vals_old || quote_literal(session_user) || ',';
        END IF;

        IF (TG_OP != 'INSERT') THEN
            v_sql := format('EXECUTE table_log_pl_%s(%s%L, %L, %L)', TG_RELID, v_vals_old, TG_OP, current_timestamp, 'old');
            EXECUTE v_sql;
        END IF;
        IF (TG_OP != 'DELETE') THEN
            v_sql := format('EXECUTE table_log_pl_%s(%s%L, %L, %L)', TG_RELID, v_vals_new, TG_OP, current_timestamp, 'new');
            EXECUTE v_sql;
            RETURN NEW;
        ELSE
            RETURN OLD;
        END IF;
    ELSE -- Otherwise we can do the inserts dynamically.
        IF (v_loguser) THEN
            v_col_trig := v_col_trig || ', "trigger_user"';
            v_val_trig := format('%L, ', session_user);
        END IF;
        v_col_trig := v_col_trig || ', "trigger_mode", "trigger_changed", "trigger_tuple"';
        v_val_trig := format('%s%L, %L', v_val_trig, TG_OP, current_timestamp);
    
        IF (TG_OP != 'INSERT') THEN
            v_sql := format('INSERT INTO %I.%I (%s%s) SELECT %s, %s, ''old'' FROM (SELECT ($1::text::%I).*) t', v_nspname, v_tabname, v_cols, v_col_trig, v_cols, v_val_trig, TG_RELID::regclass);
            EXECUTE v_sql USING OLD;
        END IF;
        IF (TG_OP != 'DELETE') THEN
            v_sql := format('INSERT INTO %I.%I (%s%s) SELECT %s, %s, ''new'' FROM (SELECT ($1::text::%I).*) t', v_nspname, v_tabname, v_cols, v_col_trig, v_cols, v_val_trig, TG_RELID::regclass);
            EXECUTE v_sql USING NEW;
            RETURN NEW;
        ELSE 
            RETURN OLD;
        END IF;
    END IF;

END;
$BODY$
LANGUAGE plpgsql VOLATILE;
