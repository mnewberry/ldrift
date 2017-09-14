#!/bin/bash

# Generate timeseries of brownian bridge
#
# Requires: sr from https://github.com/Schraiber/selection
#
#   Usage: simulate-bridge.sh <n-runs-per-cpu> <n-cpus> <bridge-params>
#
# Simulates <n-runs-per-cpu> brownian bridges in each of <n-cpus> processes.
# The required argument <bridge-params> is the argument to sr -b. See the
# documentation for sr for explanation.

# SET THIS TO THE PATH TO sr
SR=~/src/selection/sr
NRUNS=$1
NCPUS=$2
PARAMS=$3

if [[ ! -e local/out/sr/ ]] ; then mkdir -p local/out/sr ; fi

echo Simulating $NRUNS runs of sr -R -b $PARAMS...

function run {
  while [[ $((NRUNS > 0)) == 1 ]] ; do
    TMP=`mktemp`
    # Initialize seed from /dev/urandom because default seed is crappy
    SEED=`head -1 /dev/urandom | cksum | cut -d ' ' -f 1`
    $SR -R -e $SEED -b $PARAMS > $TMP
    mv $TMP local/out/sr/$PARAMS-$SEED-`md5sum $TMP | cut -d ' ' -f 1`
    NRUNS=$((NRUNS - 1))
  done 
}

# Job control noise
# Might leave up to n-cpus 0-size files in /tmp
PIDS=""
while [[ $((NCPUS > 0)) == 1 ]] ; do
  run &
  PIDS="$PIDS $!"
  NCPUS=$((NCPUS - 1))
done 

function clean_up {
  kill -9 -$$
}

trap clean_up SIGINT

for PID in $PIDS ; do wait $PID ; done ;
