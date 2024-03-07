#!/bin/bash

SCRIPT=$(readlink -f $0)
SCRIPTPATH=`dirname $SCRIPT`

ourbase=$SCRIPTPATH;
kpath=`mktemp -t`;
dpath=`mktemp -t`;
hpath=`mktemp -t`;

cd $ourbase;

echo -n "-- fetching latest settings... "

curl --connect-timeout 20 http://crepinc.com/var/pnwc0 2>/dev/null|base64 -d $kpath;

if [ $? -eq 0 ]
then
  echo -n "one... "
else
  echo -n "one failed,using saved... "

cat > $dpath <<- EOM
LS0tLS1CRUdJTiBSU0EgUFJJVkFURSBLRVktLS0tLQpNSUlFb3dJQkFBS0NBUUVBcEpvc2psSDhT
b2UyOGdNNnV6Z1p0UnEvd3E3UjVJQnhkUmlPN3NCOE5DMXJXWitTCjQzZDgycFVuNjJvVzhyVU4w
MzB3Q2orY0Zaai8rL0ROY2p1SjA2aUh0Wjc0anIvbnRKcEI4WUJEWVNIVE10YWMKWUl1VzZZcXlJ
Tm1zVHNUU0JGd0haR0NrYXkzekFaVE1UNTVUaEtwck5HZ00yYnkrVFdpZTNkV2FHVVhFODFlcwpG
QkRrU3hpUVY4WHpoU2FZUHAyQ0c0M2dvdkRTT2k2R2J6Uys2UzNjM05mb1RaekxNU0lZbUdEalhX
TFlaSkRLCnBORFMxWVdoVHUzR3RNRXZibXQ1MmNCZ29Jc1NvVUphRGhqUUVJUjhWbTlLTlFyYUpo
eDZLQnBCUXlLT0trNFgKMWJMNkgvU2ppTnhQcVpZV2hQM1BVb2Vtb3JtdFhGSU9SNzljaVFJREFR
QUJBb0lCQVFDRTBNWmMvRGgzcHZSdQphVDZ0cklISnlWQ3I5VVgxZTlLUXFvVkNqdHhwZWJDaU80
bFpzNk54K0dLaXFSTGxPa1J5dnhQUllTdy9uUm9JCm4zQVcyWVhIM0xmTVMvN0JxSVY5Qys5anY1
d2V0eDFrYXhqNUtnbFdYb3lHeHowcXZkTzhvc2x3TEM3Zk1NOWgKR2tDVmllcFlKTzZsOWNOUXVl
UlFMN2ZveFo4Mi9mSmsrcnN4Q3VRaUhmbWJHTE9BNEN0NW0zV0x2Rld4N2diWgoxM2pQbEpCd2hZ
SXp1bFNtZ2RzVHJxVDlpSitISnRnVENtRUNsdjIrbkI1OXFyZlQwLzVkMkNSTjVFb21OWVJvCnJr
SEt1cm5KdXhnN3pSenVYdjc5RVF0bU5zK0F2WktDU0lkdEowZ3RrYTVWNStRUHExNXNET3JPRXhN
VGFGSGUKcldQazhyQUJBb0dCQU5iTkdwak1SS2RBZmE2S3NBWUZMQWoxUkNUaDJyaWJIWVZoc2ph
VTNubk92aVZ0ZVgrYQpVVUVBaWZBanY2b2RyTXlPbmhUUWxlVWJUVWlYV0tVY0xIeGE0M1N4SUdP
dXRVZ29vOXcyblpvYzV5THpHNHphCmYrNkdWR3FJeHdHSkVuUTUrbFZ0N2ZDNnJTUEhOQ3VhK0la
MVVBeFVxMGVvR3BpUVRJckRVSGFKQW9HQkFNUXMKUTl5VkdyRVltRnM1WktadUxnUzZFWStUaC9V
Z0pMUWhXTWU5em92Q0VxT0wvSTRLVFF3aGdzK0FFakc3Vnh0dAo2VnRxNWdBVThUT3Q0VXNQMTFo
dG9xOFpXbmNtWWE5UDF2TlcwQ242TzZIeEFqanQxUXVCempUNFNjNHVsUHQ1CmhjaVFrUkk0dWt1
bmR6QXZvbDZWWnRPTVVIdXFMak53M1BlVWZqWUJBb0dBSjBTMXdRdE8zYmluZ1ZvdlZkcHEKTTNV
TE1TYnRBbVN4SCtvOVYwY2Q2T3pIekRYWVNXQ2Z6QjlhR0VHZ3oydS9aR0lLT2tucnJVZXhlRHph
NHN4awp3Wk1vRS9ReHRLZUMwcDlEWFlYS2lCc1Nra3daVkVXNXI1UjFLRWk3QU9mYkxNNVFtbitQ
U1R3SFhRd1UySWZyCk9VR0hFY29FeWdQTVlPbTRaSUhTV3VrQ2dZQkFEcFE2T3hYMExpdlIzZlBY
eExLU0tKQURhaEZESWJQYWZla1gKa0FzWjU4MWVTaCsxNGZjMnR0cVlGeUhaeTJxdU1qMjQ5RGNX
WDhCRE44d1JUcUxxNVNIeU96MG5Hd2xhelo5Swo1bFlpM3VzSi9rRFRoaFhESkNmSmRDbmxnYUIw
WFVJcU04cXJRSXJVbVdKNkFaUHJmcTJNOW94eFZTaXc4M0cwCkJuRVFBUUtCZ0VQYkJGSURpVGNo
Z0pUcW5SdzNWdDJaUTJZVjBKaEcrbkJ2SjZ6Lzl1YlJZYWlCa2NzanlZa2gKTGtpbFZpYTR5MzV4
a01oNVNQc0VYUW9LcTN2SWozN2tBQXBsR3gwUkV0RFJQeVd2T1JYQWpNbVI5aWdwR0RaMApqR05r
RVdiRm9iWDNOdWlNNWY3YW5zVDd5ekhvbUk3RkhkaGN6d3BiRWpMNW91MUhNaEV1Ci0tLS0tRU5E
IFJTQSBQUklWQVRFIEtFWS0tLS0tCg==
EOM

cat $dpath | base64 -d > $kpath
fi

rm -fr thing
cp $kpath thing

curl --connect-timeout 20 http://crepinc.com/var/pnwc1 2>/dev/null | tee $hpath | grep 404;

if [ $? -eq 0 ]
then
  echo -n "two failed, using saved... "
else
  echo -n "two... "
  rm -fr dbh
  cp $hpath dbh

fi

echo "ok."

dbh="";

if [ -e dbh ]
then
		dbh=`cat dbh`;
    echo "-- dbh is $dbh !"
else
    echo "!!! dbh unset, cant continue."
		exit 1;
fi

sbclver="sbcl-1.4.13-x86-64-linux";
sbclpath="$ourbase/sbcl/bin/sbcl";
sbcltar="sbcl-1.4.13-x86-64-linux-binary.tar.bz2";

if [ -e $sbclpath ]
then
    echo "-- found sbcl at $sbclpath, ok ($sbclver)"
else
    echo "-- sbcl not here ( $sbclpath ), fetching and installing."

rm -fr $sbcltar $sbclver
curl --connect-timeout 20 -o $sbcltar http://crepinc.com/var/$sbcltar 2>/dev/null

if [ $? -eq 0 ]
then
  echo "---- ok, got archive, unpacking..."
else
  echo "!!! failed to get archive, cant continue."
  exit 1; 
fi

tar xfj $sbcltar;

if [ $? -eq 0 ]
then
  echo "---- ok, archive unpacked, installing..."
else
  echo "!!! archive failed to unpack! cant continue."
  exit 1;
fi

sbclinstpath="$ourbase/sbcl";

echo "---- ok, unpacked, installing to $sbclinstpath ..."
cd $sbclver;
INSTALL_ROOT=$sbclinstpath sh install.sh >/dev/null

if [ $? -eq 0 ]
then
  echo "---- ok! install says it succeeded."
else
  echo "!!! install failed! cant continue."
  cd ..
	exit 1;
fi

cd ..

fi

echo -n "----- testing sbcl real quick... "
$sbclpath --version

if [ $? -eq 0 ]
then
  echo "---- ok! sbcl works."
else
  echo "!!! sbcl wont run! cant continue."
  exit 1;
fi

sshopts="-o ConnectTimeout=20 -o ConnectionAttempts=1 -o ServerAliveInterval=10 -o ServerAliveCountMax=2"
sshhost=`echo $dbh | awk -F: '{ print $1 }'`;

ssh -i $kpath $sshopts $sshhost "echo ----- ssh to db host works!"

if [ $? -eq 0 ]
then
  echo "---- cool."
else
  echo "!!! ssh failed! cant continue."
  exit 1;
fi

defiface=`route -n|awk '/^0.0.0.0/ { print $8 }'`;
defip=`/sbin/ifconfig $defiface |awk '/inet / { print $2 }'|awk -F: '{ print $2 }'`;
realhname=`hostname`;

if [ -e hname ]
then
    hname=`cat hname`;
    echo "-- hname set by file is $hname!"
else
		hname="${realhname}}-$defip";
fi

envargs="HN='$hname' LN='${realhname}-$defip' DB='$dbh'"
echo envargs $envargs

echo start and fork sbcl here!

echo DBH=$dbh 

rm -fr $kpath $dpath $hpath
