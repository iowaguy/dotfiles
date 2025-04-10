#!/usr/bin/env bash

# Pomodoro Timer
work_duration=25 # Work duration in minutes
break_duration=5 # Break duration in minutes
bin_dir="$HOME/.bin"
status_file="$bin_dir/pomodoro-status.txt"
counter_file_name="pomodoro-counter-$(date +%Y-%m-%d).txt"
counter_file_path="$bin_dir/$counter_file_name"
counter=
SOUND_FILE="$bin_dir/beep.wav"

# Define a function to handle Ctrl+C
function ctrl_c() {
    # rm "$status_file"
    printf "Pomodoro not running [%s]" "$counter" > "$status_file"
    dunstify --urgency=low "Pomodoro session finished"
    exit 0
}

# Set the trap for SIGINT (Ctrl+C)
trap ctrl_c SIGINT
function countdown() {
  local minutes=$1
  local seconds=$((minutes * 60))
  local countdown_type=$2

  while [ $seconds -ge 0 ]; do
    printf "$countdown_type %02d:%02d [$counter]\n" $((seconds / 60)) $((seconds % 60)) > "$status_file"
    sleep 1
    ((seconds--))
  done
}

function delete_old_days() {
  find "$bin_dir" -type f ! -name "$counter_file_name" -delete
}

read_or_create_counter_file() {
  if [ -f "$counter_file_path" ]; then
    counter=$(<"$counter_file_path")
    counter=$(echo "$counter" | xargs)  # Trim leading/trailing whitespace
  else
    counter=0
    echo "$counter" > "$counter_file_path"
  fi
}

increment_counter() {
  ((counter += 1))
  echo "$counter" > "$counter_file_path"
}

echo "Starting Pomodoro Timer: $work_duration minutes work, $break_duration minutes break."

while true; do
  delete_old_days
  read_or_create_counter_file
  aplay $SOUND_FILE
  dunstify --urgency=low "Work session started!"
  echo "Work session started!"
  countdown $work_duration "Work"
  dunstify --urgency=low "Work session complete! Time for a break."
  increment_counter
  aplay $SOUND_FILE
  echo -e "\nWork session complete! Time for a break."

  dunstify --urgency=low "Break session started!"
  echo "Break session started!"
  countdown $break_duration "Break"
  dunstify --urgency=low "Break session complete! Back to work."
  echo -e "\nBreak session complete! Back to work."
done
