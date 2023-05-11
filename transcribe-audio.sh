#!/usr/bin/env bash
echo "Doing transcribe audio : $@"
base_name=$(basename "$@")
REMOTE_DIR=/home/divam/scratch/transcribe
REMOTE_FILE=$REMOTE_DIR/$base_name
ssh build "
  /home/divam/scratch/sources/nixpkgs/result-whisper-cpp/bin/whisper-cpp -f $REMOTE_FILE -t 8 -m /home/divam/scratch/whisper.cpp/ggml-medium.en.bin -otxt > /dev/null
  cat $REMOTE_FILE.txt
  rm $REMOTE_FILE $REMOTE_FILE.txt
"
rm "$@"
