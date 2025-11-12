#!/usr/bin/env bash
#
# record-audio.sh - Record audio using PipeWire or PulseAudio
# Usage: ./record-audio.sh <output-file.wav>
#

set -euo pipefail

# Show help message
show_help() {
    cat << EOF
Usage: ${0##*/} [OPTIONS] <output-file.wav>

Record audio to a WAV file using available audio recording command.

OPTIONS:
    -h, --help    Show this help message and exit

EXAMPLE:
    ${0##*/} recording.wav
    ${0##*/} /path/to/output.wav

Press Ctrl+C to stop recording.
EOF
    exit 0
}

# Parse arguments
if [[ $# -eq 0 ]]; then
    echo "Error: No output file specified" >&2
    show_help
fi

if [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
    show_help
fi

OUTPUT_FILE="$1"

# Validate output directory exists and is writable
OUTPUT_DIR="$(dirname "$OUTPUT_FILE")"
if [[ ! -d "$OUTPUT_DIR" ]]; then
    echo "Error: Directory '$OUTPUT_DIR' does not exist" >&2
    exit 1
fi

if [[ ! -w "$OUTPUT_DIR" ]]; then
    echo "Error: Directory '$OUTPUT_DIR' is not writable" >&2
    exit 1
fi

# Detect available recording command
RECORD_CMD=""
if command -v pw-record &> /dev/null; then
    RECORD_CMD="pw-record --channels 1 --rate 16000"
    echo "Using PipeWire (pw-record) for recording"
elif command -v parec &> /dev/null; then
    RECORD_CMD="parec --file-format=wav --rate=16000"
    echo "Using PulseAudio (parec) for recording"
else
    echo "Error: No audio recording command found (pw-record or parec)" >&2
    echo "Please install pipewire or pulseaudio" >&2
    exit 1
fi

# Setup signal handling and cleanup
BGPID=""
cleanup() {
    if [[ -n "$BGPID" ]]; then
        echo ""
        echo "Stopping recording..."
        kill "$BGPID" 2>/dev/null || true
        wait "$BGPID" 2>/dev/null || true
    fi
    echo "Recording stopped"
    exit 0
}

trap cleanup INT TERM HUP

# Start recording
echo "Recording to: $OUTPUT_FILE"
echo "Press Ctrl+C to stop..."
$RECORD_CMD "$OUTPUT_FILE" &
BGPID=$!

# Wait for recording process
wait $BGPID
