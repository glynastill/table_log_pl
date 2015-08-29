table_log_pl
===========

Attempt at pl/pgsql drop-in replacement for the dated table_log C extenstion
AKA pg Table Audit / PostgreSQL Table Log / tablelog by Andreas Scherbaum:
     http://www.postgresql.org/ftp/projects/pgFoundry/tablelog/tablelog/
     http://github.com/andreasscherbaum/table_log

A slightly more up to date version of the original C extension can also 
be found here:
     https://github.com/glynastill/pg_table_audit

There are now many better ways to audit DML, using json types or advanced 
extensions like pgaudit (below), however if for some reason you're stuck 
with table_log this may help.
