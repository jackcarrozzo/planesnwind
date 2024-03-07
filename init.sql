dbname="planesnwind"
admin="pgsql"
user="pnwYay"

echo "drop database if exists $dbname" | psql -h 127.0.0.1 -U $user -e postgres
psql -h 127.0.0.1 -U $admin -e postgres < sql/admin.sql
createdb -O $user -h 127.0.0.1 -U $admin $dbname
psql -h 127.0.0.1 -U $user $dbname -e < sql/schema.sql 
echo '\d' | psql -h 127.0.0.1 -U $user -e $dbname

