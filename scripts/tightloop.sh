#!/bin/bash
set -e  # Exit on error

input=$1
output=$2

# Help message if first argument is --help or -?
if [ "$input" = "--help" ] || [ "$input" = "-?" ]; then
  echo "Usage: $0 <input_wav_file> <output_wav_file>"
  echo "Builds, transforms, and plays debug stages for a WAV file."
  echo "Arguments:"
  echo "  <input_wav_file>   Path to input WAV file."
  echo "  <output_wav_file>  Path prefix for output debug WAV files."
  exit 0
fi

if [ -z "$output" ] || [ -z "$input" ]; then
  echo "Usage: $0 <input_wav_file> <output_wav_file>"
  exit 1
fi

echo "Building..."
cabal build

echo "Cleaning up old debug files..."
rm -f ${output%.wav}_*.wav

echo "Transforming with debug stages..."
cabal run voice-box -- "$input" --transform --debug -o "${output%.wav}"

echo "Playing intermediate stages..."
shopt -s nullglob  # Make glob return empty if no matches
files=(${output%.wav}_*.wav)
if [ ${#files[@]} -eq 0 ]; then
  echo "No debug files found!"
  exit 1
fi

for f in "${files[@]}"; do
  echo "Playing: $f"
  aplay "$f"
  read -p "Press Enter for next file..."
done

echo "Done!"