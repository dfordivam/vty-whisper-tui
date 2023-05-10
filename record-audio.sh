#!/usr/bin/env bash
BGPID=""
trap 'kill $BGPID; exit' INT
echo "Doing record audio : $@"
parec --file-format=wav --rate=16000 "$@" &
BGPID=$!
wait $BGPID
