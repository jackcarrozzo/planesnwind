#!/bin/bash


if [ -e ncat ]
then
    echo "-- nc is here: `./ncat --version`"
else
		echo "--- netcat missing, fetching..."
		wget "https://github.com/andrew-d/static-binaries/raw/master/binaries/linux/x86_64/ncat"

if [ $? -eq 0 ]
then
  chmod +x ncat
	echo "---- ok! `./ncat --version`"
else
  echo "!!! fetch failed! cant continue."
  exit 1;
fi
fi

echo k|./ncat -w10 --send-only 127.0.0.1 5506 2>/dev/null >/dev/null

if [ $? -eq 0 ] 
then
	echo connection to db works, tunnel is fine.
else
  echo "!!! cant connect to db, bumping tunnel."
  
	pid=`ps aux|grep ssh|grep 5506|awk '{ print $2 }'`;
	ps aux|grep ssh|grep 5506 2>/dev/null >/dev/null

  if [ $? -eq 0 ]
		then
  	echo ssh tun is still there as pid $pid, killing it....

		kill $pid
		sleep 4
		kill -9 $pid 2>/dev/null >/dev/null
	else
  	echo "no ssh tun was running."
	fi

	sshopts="-o ConnectTimeout=20 -o ConnectionAttempts=1 -o ServerAliveInterval=10 -o ServerAliveCountMax=2"
	dbh=`cat dbh`;
	ssh -i thing $sshopts -N -L 5506:localhost:5432 $dbh &
	
	echo k tunnel started
fi


ps aux|grep sbcl|grep -v grep

if [ $? -eq 0 ]
then
  echo "---- sbcl running, all good."
	exit 0
else
  echo "!!! sbcl not found, starting one."



fi

