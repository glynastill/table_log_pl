EXTENSION = table_log_pl
DATA = $(wildcard sql/*--*.sql)
DOCS = README.md 

PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
