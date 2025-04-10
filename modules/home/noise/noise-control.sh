#!/usr/bin/env bash

PROCESS_NAME="noise"
bin_dir="$HOME/.bin"
COMMAND="$bin_dir/noise"
NAME=$(basename $0)
status_file="$bin_dir/noise-status.txt"

PID=$(ps aux | grep -v grep | grep -v "$NAME" | grep "$PROCESS_NAME" | awk '{print $2}')
if [ -z "$PID" ]; then
    printf "Noise on" > "$status_file"
    exec $COMMAND
else
    kill $PID
fi
