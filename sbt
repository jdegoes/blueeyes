#!/bin/sh
java -Xmx2048m -jar `dirname $0`/project/sbt-launch-0.7.4.jar "$@"
exit $?
