dbname="planesnwind"
user="pnw"
pass="postgres.rox"

sudo su postgres -c "dropdb $dbname"
sudo su postgres -c "createuser $user"
echo "alter user $user with password '$pass'" | sudo su postgres -c "psql -e postgres"
echo "alter role $user with login" | sudo su postgres -c "psql -e postgres"
sudo su postgres -c "createdb -O $user $dbname"
cat sql/admin.sql | sudo su postgres -c "psql -e $dbname"
cat sql/schema.sql | PGPASSWORD=$pass psql -h 127.0.0.1 -U $user -e $dbname
echo '\d' | PGPASSWORD=$pass psql -h 127.0.0.1 -U $user -e $dbname

createuser planesnwind
echo "alter user planesnwind with password 'yaymath'" | psql -e postgres
echo "alter role planesnwind with login" | psql -e postgres
createdb -O planesnwind planesnwind
