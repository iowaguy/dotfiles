#!/usr/bin/env bash

# Pomodoro Timer
work_duration=25 # Work duration in minutes
break_duration=5 # Break duration in minutes
status_file="$HOME/.bin/pomodoro-status.txt"

# Define a function to handle Ctrl+C
function ctrl_c() {
    # echo -ne "Pomodoro not running" > "$status_file"
    rm "$status_file"
    exit 0
}

# Set the trap for SIGINT (Ctrl+C)
trap ctrl_c INT

function countdown() {
  local minutes=$1
  local countdown_type=$2
  while [ $minutes -gt 0 ]; do
    echo -ne "$minutes minute(s) remaining...\r"
    echo -ne "$countdown_type $minutes min remaining" > "$status_file"
    sleep 60
    ((minutes--))
  done
}

echo "Starting Pomodoro Timer: $work_duration minutes work, $break_duration minutes break."

while true; do
  dunstify --urgency=low "Work session started!"
  echo "Work session started!"
  countdown $work_duration "Work"
  dunstify --urgency=low "Work session complete! Time for a break."
  echo -e "\nWork session complete! Time for a break."

  dunstify --urgency=low "Break session started!"
  echo "Break session started!"
  countdown $break_duration "Break"
  dunstify --urgency=low "Break session complete! Back to work."
  echo -e "\nBreak session complete! Back to work."
done
