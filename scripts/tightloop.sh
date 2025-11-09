#!/bin/bash
set -e  # Exit on error

echo "Building..."
cabal build

echo "Cleaning up old debug files..."
rm -f predoll0_*.wav

echo "Transforming with debug stages..."
cabal run voice-box -- --transform --debug FirstLine.wav -o predoll0.wav

echo "Playing intermediate stages..."
for f in predoll0_*.wav; do
  if [ -f "$f" ]; then
    echo "Playing $f"
    aplay "$f"
  fi
done

echo "Done!"