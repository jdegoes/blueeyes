#!/bin/sh
echo '{"name":"Sherlock","email":"sherlock@email.com","country":"UK","city":"London","address":"Baker Street, 221B"}' > contact.txt

if [ -z $2 ]; then
  echo "Usage: concurrency requests [expected rate]"
  exit 1
fi

CONCURRENCY=$1
REQUESTS=$2
RATE=$3

if [ -z $RATE ]; then RATE=1000; fi;


function benchmark {
  init
  command=$1  

  echo -e '\E[37;44m'"\033[1mSTART BENCHMARK\033[0m"
  echo -e '\E[37;44m'"\033[1m*******************************************\033[0m"
  echo -e '\E[37;44m'"\033[1m"$command"\033[0m"
  echo -e '\E[37;44m'"\033[1m*******************************************\033[0m" 
  eval $command

  checkRate ab.txt
  cleanUp
}

function cleanUp {
  curl -X DELETE http://localhost:8585/contacts/Sherlock
}

function init {
  ab -c 1 -n 1 -p contact.txt http://localhost:8585/contacts > tmp.txt
}

function checkRate {
  tail -n 200 $1
  rateLine=`grep "Requests" $1`
  currentRate=`echo $rateLine|sed 's/.* \([[:digit:]]\{1,\}\).*/\1/'`
  if [ $RATE -ge  $currentRate ]; then 
    
    echo -e '\E[47;31m'"\033[1m*******************************************\033[0m"
    echo -e '\E[47;31m'"\033[1mFAILED                           \033[0m"
    echo -e '\E[47;31m'"\033[1mEXPECTED REQUESTS = "$RATE"                   \033[0m"
    echo -e '\E[47;31m'"\033[1mACTUAL REQUESTS   = "$currentRate"                   \033[0m"
    echo -e '\E[47;31m'"\033[1m*******************************************\033[0m"
  else
    echo -e '\E[37;44m'"\033[1m*******************************************\033[0m" 
    echo -e '\E[37;44m'"\033[1mSUCCESSFUL\033[0m" 
    echo -e '\E[37;44m'"\033[1mEXPECTED REQUESTS   = "$RATE"\033[0m" 
    echo -e '\E[37;44m'"\033[1mACTUAL REQUESTS = "$currentRate"\033[0m" 
    echo -e '\E[37;44m'"\033[1m*******************************************\033[0m" 
  fi   
}

benchmark "ab -c $CONCURRENCY -n $REQUESTS -p contact.txt http://localhost:8585/contacts > ab.txt"
echo ""
echo ""
benchmark "ab -c $CONCURRENCY -n $REQUESTS http://localhost:8585/contacts > ab.txt"

rm ab.txt
rm contact.txt
rm tmp.txt

