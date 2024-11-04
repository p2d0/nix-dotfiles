#!/usr/bin/env python3
import os
import time
import signal
from datetime import datetime, timedelta
from threading import Thread

# Configuration (modify these as needed)
STANDBY_LABEL = "Ready"
SLEEP_DURATION = 1  # Update interval in seconds
SOUND_FILE = os.path.expanduser("~/.config/polybar/mixkit-achievement-bell-600.wav")  # Adjust path if necessary

# In-memory data structures
timer_data = {}
stopwatch_data = {}
pomo_count = 0

def get_now():
    """Returns the current time in seconds since epoch (UTC)."""
    return int(time.time())

def kill_timer():
    """Clears timer data."""
    global timer_data
    timer_data = {}

def kill_stopwatch():
    """Stops the stopwatch if running."""
    if 'pid' in stopwatch_data:
        os.kill(stopwatch_data['pid'], signal.SIGTERM)  # Use SIGTERM for graceful termination
    stopwatch_data = {}

def reset_count():
    """Resets the timer count."""
    global pomo_count
    pomo_count = 0

def timer_running():
    """Checks if a timer is currently running."""
    return bool(timer_data)

def timer_count():
    """Reads the current timer count."""
    return pomo_count

def decrement_pomo_count():
    """Decrements the timer count."""
    global pomo_count
    pomo_count -= 1

def increment_pomo_count(interval):
    """Increments the timer count, considering the interval."""
    global pomo_count
    pomo_count += interval // 60

def timer_expiry():
    """Reads the timer expiry time in seconds since epoch."""
    return timer_data.get('expiry')

def timer_label():
    """Reads the timer label."""
    return timer_data.get('label')

def timer_action():
    """Reads the timer action command."""
    return timer_data.get('action')

def timer_length():
    """Calculates the remaining timer length in minutes."""
    if timer_running():
        return (timer_expiry() - get_now()) // 60
    else:
        return 0

def stopwatch_running():
    """Checks if the stopwatch is running."""
    return 'pid' in stopwatch_data

def get_stopwatch_time():
    """Returns the elapsed stopwatch time in minutes:seconds format."""
    if stopwatch_running():
        elapsed_seconds = stopwatch_data['elapsed_time']
        return f"{elapsed_seconds // 60}:{elapsed_seconds % 60}"
    else:
        return "0:0"

def update_tail():
    if timer_running() and get_now() >= timer_expiry():
        play_sound()
        execute_timer_action()
        kill_timer()

    if stopwatch_running():
        print(f"{pomo_count} Stopwatch running: {get_stopwatch_time()}")
    elif timer_running():
        print(f"{pomo_count} {timer_label()} {timer_length() // 60}:{timer_length() % 60}")
    else:
        print(f"{pomo_count} {STANDBY_LABEL}")

def play_sound():
    if os.path.exists(SOUND_FILE):
        os.system(f"play {SOUND_FILE}")

def execute_timer_action():
    action = timer_action()
    if action:
        os.system(action)

def start_stopwatch():
    global stopwatch_data, pomo_count
    def stopwatch_thread():
        interval = 1500  # 25 minutes in seconds
        increment_pomo_count(interval)
        count = 0
        while True:
            time.sleep(1)
            count += 1
            stopwatch_data['elapsed_time'] = count
            if count >= interval * 2:
                increment_pomo_count(interval)
                count = 0

    stopwatch_thread = Thread(target=stopwatch_thread, daemon=True)
    stopwatch_thread.start()
    stopwatch_data['pid'] = stopwatch_thread.ident

def stop_stopwatch():
    global stopwatch_data
    kill_stopwatch()

# Main logic for Polybar integration
if __name__ == "__main__":
    import sys

    mode = sys.argv[1]
    if mode == "tail":
        standby_label = sys.argv[2]
        update_interval = int(sys.argv[3])
        while True:
            update_tail()
            time.sleep(update_interval)
    elif mode == "new":
        length = int(sys.argv[2])
        label = sys.argv[3]
        action = sys.argv[4]
        kill_stopwatch()
        kill_timer()
        set_timer_data(length, label, action)
    elif mode == "increase":
        increase_time = int(sys.argv[2])
        if timer_running():
            timer_data['expiry'] += increase_time
    elif mode == "reset_count":
        kill_stopwatch()
        reset_count()
    elif mode == "decrement_count":
        kill_stopwatch()
        decrement_pomo_count()
    elif mode == "cancel":
        kill_stopwatch()
        kill_timer()
    elif mode == "stopwatch":
        if stopwatch_running():
            stop_stopwatch()
        else:
            start_stopwatch()
    else:
        print("Invalid mode")
