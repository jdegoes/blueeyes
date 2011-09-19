#!/bin/sh
LOG='tee -a FastWriteMapBench.txt'

# clear the timing log
echo "Benchmark of FastWriteMap, started $(date)" | $LOG
echo "timing FastWriteMap performing 200,000 adds/overwrites 10x" | $LOG
sbt "test:run-main blueeyes.structures.FastWriteMapBench 10 20" | $LOG
echo "timing immutable HashMap performing 200,000 adds/overwrites 10x" | $LOG
sbt "test:run-main blueeyes.structures.HashMapBench 10 20" | $LOG
echo "timing poor-performing FastWriteMap performing 200,000 adds/overwrites on an out-of-date map; causing lots of copying of maps and histories" | $LOG
sbt "test:run-main blueeyes.structures.SlowWriteMapBench 10 20" | $LOG

