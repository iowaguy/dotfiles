#!/usr/bin/env bash

PROCESS_NAME="pomodoro"
COMMAND="$HOME/.bin/pomodoro"
NAME=$(basename $0)
bin_dir="$HOME/.bin"
status_file="$bin_dir/pomodoro-status.txt"
counter_file_name="pomodoro-counter-$(date +%Y-%m-%d).txt"
counter_file_path="$bin_dir/$counter_file_name"
counter=


read_or_create_counter_file() {
  if [ -f "$counter_file_path" ]; then
    counter=$(<"$counter_file_path")
    counter=$(echo "$counter" | xargs)  # Trim leading/trailing whitespace
  else
    counter=0
    echo "$counter" > "$counter_file_path"
  fi
}

PID=$(ps aux | grep -v grep | grep -v "$NAME" | grep "$PROCESS_NAME" | awk '{print $2}')
if [ -z "$PID" ]; then
    exec $COMMAND
else
    dunstify --urgency=low "Pomodoro session finished"
    kill $PID
    # Clear focus sinkholes if the NixOS pomodoroFocusDns helper is installed.
    if command -v pomodoro-dns-apply >/dev/null 2>&1; then
        sudo -n pomodoro-dns-apply off 2>/tmp/pomodoro-dns-apply.err || true
    fi
    read_or_create_counter_file
    printf "Pomodoro not running [%s]" "$counter" > "$status_file"
    # rm $status_file
fi
