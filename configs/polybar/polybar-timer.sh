#!/bin/sh

### AUTHOR: Johann Birnick (github: jbirnick)
### PROJECT REPO: https://github.com/jbirnick/polybar-timer

## FUNCTIONS

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

now () { date --utc +%s; }

killTimer () {
  rm -f /tmp/polybar-timer/expiry
  rm -f /tmp/polybar-timer/label
  rm -f /tmp/polybar-timer/action
}

resetCount () {
  echo "" > $script_dir/count
}

timerRunning () { [ -e /tmp/polybar-timer/expiry ] ; }

timerCount () { cat $script_dir/count ; }

decrementPomoCount() {
  if [ "$(timerLength)" -gt 15 ]; then
    current_count=$(cat $script_dir/count)
    new_count=$((current_count - 1))
    echo "$new_count" > $script_dir/count
  fi
}

incrementPomoCount() {
  if [ "${1}" -gt 15 ]; then
    current_count=$(cat $script_dir/count)
    new_count=$((current_count + 1))
    echo "$new_count" > $script_dir/count
  fi
}

timerExpiry () { cat /tmp/polybar-timer/expiry ; }
timerLabel () { cat /tmp/polybar-timer/label ; }
timerAction () { cat /tmp/polybar-timer/action ; }
timerLength () { cat /tmp/polybar-timer/length ; }

secondsLeft () { echo $(( $(timerExpiry) - $(now) )) ; }
minutesLeft () { echo $(( ( $(secondsLeft) ) / 60 )) ; }
secondsInAMinuteLeft () { echo $(( $(secondsLeft) % 60)); }

printExpiryTime () { dunstify -u low -r -12345 "Timer expires at $( date -d "$(secondsLeft) sec" +%H:%M)" ;}

deleteExpiryTime () { dunstify -C -12345 ; }

stopwatchRunning() { [ -e /tmp/polybar-timer/stopwatch_pid ] ; }

getStopwatchTime() {
  if stopwatchRunning; then
    elapsed_seconds=$(cat /tmp/polybar-timer/stopwatch_time)
    echo "$((elapsed_seconds / 60)):$((elapsed_seconds % 60))"
  else
    echo "0:0"
  fi
}

updateTail () {
  if timerRunning && [ $(secondsLeft) -le 0 ]
  then
    paplay ~/.config/polybar/mixkit-achievement-bell-600.wav;notify-send -u critical "Timer expired."
    eval $(timerAction)
    killTimer
  fi

  if stopwatchRunning
  then
    elapsed_seconds=$(cat /tmp/polybar-timer/stopwatch_time)
    echo "$(timerCount) Stopwatch running: $((elapsed_seconds / 60)):$((elapsed_seconds % 60))"
  elif timerRunning
  then
    echo "$(timerCount) $(timerLabel) $(minutesLeft):$(secondsInAMinuteLeft)"
  else
    echo "$(timerCount) ${STANDBY_LABEL}"
  fi
}

## MAIN CODE

case $1 in
  tail)
    STANDBY_LABEL=$2
    SLEEP_DURATION=${3}

    trap updateTail USR1

    while true
    do
      updateTail
      if [ -z "$SLEEP_DURATION" ]; then
        echo "Error: Sleep duration is missing."
        exit 1
      fi
      sleep "$SLEEP_DURATION" &
      wait
    done
    ;;
  update)
    kill -USR1 $(pgrep --oldest --parent ${2})
    ;;
  new)
    if timerRunning
    then
      killTimer
      decrementPomoCount
      deleteExpiryTime
    else
      killTimer
      mkdir -p /tmp/polybar-timer
      echo "${2}" > /tmp/polybar-timer/length
      echo "$(( $(now) + 60*${2} ))" > /tmp/polybar-timer/expiry
      echo "${3}" > /tmp/polybar-timer/label
      echo "${4}" > /tmp/polybar-timer/action
      incrementPomoCount ${2}
      printExpiryTime
    fi
    ;;
  increase)
    if timerRunning
    then
      echo "$(( $(cat /tmp/polybar-timer/expiry) + ${2} ))" > /tmp/polybar-timer/expiry
    else
      exit 1
    fi
    printExpiryTime
    ;;
  reset_count)
    resetCount
    ;;
  cancel)
    killTimer
    deleteExpiryTime
    ;;
  stopwatch)
    if stopwatchRunning
    then
      kill $(cat /tmp/polybar-timer/stopwatch_pid)
      rm /tmp/polybar-timer/stopwatch_pid
      rm /tmp/polybar-timer/stopwatch_time
      echo "Stopwatch stopped."
    else
      (
        interval=1500 # 25 minutes in seconds
        count=0
        while true
        do
          sleep 1
          count=$((count + 1))
          echo $count > /tmp/polybar-timer/stopwatch_time
          if [ $count -ge $interval ]
          then
            incrementPomoCount $interval
            count=0
          fi
        done
      ) &
      echo $! > /tmp/polybar-timer/stopwatch_pid
      echo 0 > /tmp/polybar-timer/stopwatch_time
      echo "Stopwatch started."
    fi
    ;;
  *)
    echo "Please read the manual at https://github.com/jbirnick/polybar-timer ."
    ;;
esac
