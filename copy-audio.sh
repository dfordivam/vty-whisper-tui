#!/usr/bin/env bash
echo "Doing copy audio : $@"
REMOTE_DIR=/home/divam/scratch/transcribe
scp "$@" build:$REMOTE_DIR/
