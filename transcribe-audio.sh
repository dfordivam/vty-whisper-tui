#!/usr/bin/env bash
echo "Doing transcribe audio : $@"
/home/divam/scratch/sources/nixpkgs/result-whisper-cpp/bin/whisper-cpp -f "$@"  -t 8 -m /home/divam/scratch/whisper.cpp/ggml-small.en.bin -otxt
cat "$@.txt"
rm "$@" "$@.txt"
