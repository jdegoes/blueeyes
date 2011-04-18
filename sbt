#!/bin/sh
java -Xmx2048m -XX:MaxPermSize=512m -jar `dirname $0`/project/sbt-launch-0.7.5.jar "$@"
exit $?
