# vty-whisper-tui

This is a CLI/TUI app for doing semi real-time transcription of speech (STT).
It currently makes use of [whisper-cpp](https://github.com/ggml-org/whisper.cpp), but could be modified to use any other tool which can transcribe an audio file.

Doing STT in real time on a laptop is actually hard as the CPU gets over-loaded, and requires reducing the quality of model being used.
But semi real time allows use of a recorded file and a more powerful model.
There is an additional benefit of being able to do the processing on a remote machine having faster CPU.

Another feature of this app is the ability to manually do one sentence at a time.
The built-in streaming feature for real time speech processing has a much higher error rate because it tends to chunks the audio based on a fixed length.

If the chunking happens in the middle of the sentence then certain words are either not transcribed or they are transcribed wrong.
This is very annoying in practice as it is fairly easy to forget what was spoken and miss parts of the content.

### Usage

In order to use this atleast these two scripts need to be set up

- ./record-audio.sh: Uses either `pw-record` or `parec` to record audio from the default source to a given file. modify this as necessary for your system.

- Either ./local-transcribe-audio.sh, Or ./copy-audio.sh and ./transcribe-audio.sh

Optionally `./copy-to-clipboard.sh` may be used.

Once these scripts work on your system, start the app using nix-shell

`nix-shell --run 'cabal run'`




