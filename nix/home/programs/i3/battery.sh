#!/usr/bin/env bash

BAT=$(acpi -b | grep -E -o '[0-9]?[0-9][0-9]?%')

# Full and short texts
echo "Battery: $BAT"
echo "BAT: $BAT"

# Set urgent flag below 5%
[ ${BAT%?} -le 5 ] && exit 33

# Set orange below 20%
[ ${BAT%?} -le 20 ] && echo "#FF8000"

# Set green if battery is full
[ ${BAT%?} -eq 100 ] && echo "#00FA6C"
exit 0
