#!/bin/sh

### AUTHOR:         Johann Birnick (github: jbirnick)
### PROJECT REPO:   https://github.com/jbirnick/polybar-timer

## FUNCTIONS

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

now () { date --utc +%s; }

killTimer () {
  rm /tmp/polybar-timer/expiry ;
  rm /tmp/polybar-timer/label ;
  rm /tmp/polybar-timer/action ;
}

resetCount (){
  cat "" > $script_dir/count;
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

updateTail () {
  if timerRunning && [ $(secondsLeft) -le 0 ]
  then
    paplay ~/.config/polybar/mixkit-achievement-bell-600.wav;notify-send -u critical \"Timer expired.\"
    eval $(timerAction)
    killTimer
  fi

  if timerRunning
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

    trap updateTail USR1

    while true
     do
     updateTail
     sleep ${3} &
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
      mkdir /tmp/polybar-timer
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
  *)
    echo "Please read the manual at https://github.com/jbirnick/polybar-timer ."
    ;;
esac
