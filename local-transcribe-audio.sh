#!/usr/bin/env bash

set -euo pipefail

# Default configuration
VTY_WHISPER_MODEL="${VTY_WHISPER_MODEL:-/home/divam/scratch/whisper.cpp/ggml-distil-large-v3.bin}"
VTY_WHISPER_THREADS="${VTY_WHISPER_THREADS:-8}"
VTY_WHISPER_USE_VULKAN="${VTY_WHISPER_USE_VULKAN:-false}"
VTY_WHISPER_VERBOSE="${VTY_WHISPER_VERBOSE:-false}"
VTY_WHISPER_CLEANUP="${VTY_WHISPER_CLEANUP:-true}"

# Show help message
show_help() {
    cat <<EOF
Usage: $0 [OPTIONS] <audio_file>

Transcribe audio file using whisper-cpp from nixpkgs.

OPTIONS:
    -h, --help              Show this help message
    -v, --verbose           Enable verbose output
    --no-cleanup            Don't remove audio and transcript files after processing

ENVIRONMENT VARIABLES:
    VTY_WHISPER_MODEL       Path to whisper model file (required)
                            Default: /home/divam/scratch/whisper.cpp/ggml-distil-large-v3.bin
    VTY_WHISPER_THREADS     Number of threads to use
                            Default: 8
    VTY_WHISPER_USE_VULKAN  Use Vulkan-accelerated whisper build (true/false)
                            Default: false (auto-set by nix-shell with --arg enableVulkan true)
    VTY_WHISPER_VERBOSE     Enable verbose whisper output (true/false)
                            Default: false
    VTY_WHISPER_CLEANUP     Remove audio and output files after processing (true/false)
                            Default: true

EXAMPLES:
    # Enter nix-shell first (CPU-only)
    nix-shell run.nix
    $0 audio.wav

    # Use Vulkan acceleration
    nix-shell run.nix --arg enableVulkan true
    $0 audio.wav

    # Use custom model
    VTY_WHISPER_MODEL=/path/to/model.bin $0 audio.wav

    # Keep files after processing
    $0 --no-cleanup audio.wav

EOF
}

# Parse command line arguments
AUDIO_FILE=""
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            exit 0
            ;;
        -v|--verbose)
            VTY_WHISPER_VERBOSE=true
            shift
            ;;
        --no-cleanup)
            VTY_WHISPER_CLEANUP=false
            shift
            ;;
        -*)
            echo "Error: Unknown option: $1" >&2
            show_help
            exit 1
            ;;
        *)
            AUDIO_FILE="$1"
            shift
            ;;
    esac
done

# Validate input
if [[ -z "$AUDIO_FILE" ]]; then
    echo "Error: No audio file specified" >&2
    show_help
    exit 1
fi

if [[ ! -f "$AUDIO_FILE" ]]; then
    echo "Error: Audio file not found: $AUDIO_FILE" >&2
    exit 1
fi

if [[ ! -f "$VTY_WHISPER_MODEL" ]]; then
    echo "Error: Model file not found: $VTY_WHISPER_MODEL" >&2
    echo "Set VTY_WHISPER_MODEL environment variable to the correct path" >&2
    exit 1
fi

# Redirect output if not verbose
if [[ "$VTY_WHISPER_VERBOSE" == "true" ]]; then
    echo "Transcribing audio: $AUDIO_FILE"
    echo "Model: $VTY_WHISPER_MODEL"
    echo "Threads: $VTY_WHISPER_THREADS"
    echo "Vulkan: $VTY_WHISPER_USE_VULKAN"
    whisper-cli -f "$AUDIO_FILE" -t "$VTY_WHISPER_THREADS" -m "$VTY_WHISPER_MODEL" -otxt
else
    echo "Doing transcribe audio: $AUDIO_FILE"
    whisper-cli -f "$AUDIO_FILE" -t "$VTY_WHISPER_THREADS" -m "$VTY_WHISPER_MODEL" -otxt > /dev/null 2>&1
fi

# Check if transcription was successful
OUTPUT_FILE="$AUDIO_FILE.txt"
if [[ ! -f "$OUTPUT_FILE" ]]; then
    echo "Error: Transcription failed, output file not found: $OUTPUT_FILE" >&2
    exit 1
fi

# Display output
cat "$OUTPUT_FILE"

# Cleanup if requested
if [[ "$VTY_WHISPER_CLEANUP" == "true" ]]; then
    rm -f "$AUDIO_FILE" "$OUTPUT_FILE"
fi
