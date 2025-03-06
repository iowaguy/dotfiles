#!/usr/bin/env bash

PROCESS_NAME="pomodoro"
COMMAND="$HOME/.bin/pomodoro"
NAME=$(basename $0)
status_file="$HOME/.bin/pomodoro-status.txt"

PID=$(ps aux | grep -v grep | grep -v "$NAME" | grep "$PROCESS_NAME" | awk '{print $2}')
if [ -z "$PID" ]; then
    exec $COMMAND
else
    dunstify --urgency=low "Pomodoro session finished"
    kill $PID
    rm $status_file
fi
