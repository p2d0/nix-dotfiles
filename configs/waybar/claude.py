#!/usr/bin/env python3
import json
import os
import sys
import requests
from datetime import datetime, timezone

# ==============================================================================
#  CONFIGURATION
# ==============================================================================
CREDENTIALS_FILE = os.path.expanduser("/mnt/pc/home/andrew/.claude/.credentials.json")
CACHE_FILE = "/tmp/claude_usage_cache.json"
CACHE_TTL = 300  # seconds (5 minutes)
# ==============================================================================


def get_token():
    try:
        with open(CREDENTIALS_FILE) as f:
            data = json.load(f)
        oauth = data["claudeAiOauth"]
        expires_at = oauth.get("expiresAt", 0)
        # expiresAt is in milliseconds (JS timestamp)
        if expires_at > 1e12:
            expires_at /= 1000
        if datetime.now(timezone.utc).timestamp() > expires_at:
            return None, "Token Expired - reopen Claude Code to refresh"
        return oauth["accessToken"], None
    except FileNotFoundError:
        return None, f"Credentials not found in {CREDENTIALS_FILE}"
    except Exception as e:
        return None, str(e)


def get_cached():
    try:
        with open(CACHE_FILE) as f:
            cache = json.load(f)
        if datetime.now(timezone.utc).timestamp() - cache["timestamp"] < CACHE_TTL:
            return cache["data"]
    except Exception:
        pass
    return None


def save_cache(data):
    try:
        with open(CACHE_FILE, "w") as f:
            json.dump(
                {"timestamp": datetime.now(timezone.utc).timestamp(), "data": data}, f
            )
    except Exception:
        pass


def get_progress_bar(percent, length=10):
    filled = int(length * float(percent) / 100)
    return "■" * filled + "□" * (length - filled)


def usage_color(percent):
    p = float(percent)
    if p < 40:
        return "#a6e3a1"  # green
    elif p < 70:
        return "#f9e2af"  # yellow
    elif p < 90:
        return "#fab387"  # orange
    return "#f38ba8"  # red


def format_reset(resets_at_str):
    if not resets_at_str:
        return "no active session"
    try:
        reset_dt = datetime.fromisoformat(resets_at_str.replace("Z", "+00:00"))
    except (AttributeError, ValueError):
        return "?"
    diff = reset_dt - datetime.now(timezone.utc)
    total = int(diff.total_seconds())
    if total <= 0:
        return "now"
    h, rem = divmod(total, 3600)
    m = rem // 60
    if h > 0:
        return f"{h}h {m}min"
    return f"{m}min"


def fetch_usage(token):
    cached = get_cached()
    if cached:
        return cached, None
    try:
        resp = requests.get(
            "https://api.anthropic.com/api/oauth/usage",
            headers={
                "Authorization": f"Bearer {token}",
                "anthropic-beta": "oauth-2025-04-20",
                "User-Agent": "claude-code/2.0.32",
            },
            timeout=10,
        )
        resp.raise_for_status()
        data = resp.json()
        if "error" in data:
            return None, str(data["error"])
        save_cache(data)
        return data, None
    except Exception as e:
        return None, str(e)


def build_output(data):
    five = data.get("five_hour", {})
    seven = data.get("seven_day", {})
    extra = data.get("extra_usage", {})

    five_pct = round(float(five.get("utilization") or 0))
    seven_pct = round(float(seven.get("utilization") or 0))

    five_reset = format_reset(five.get("resets_at"))
    seven_reset = format_reset(seven.get("resets_at"))

    five_color = usage_color(five_pct)
    seven_color = usage_color(seven_pct)

    # --- BAR TEXT ---
    # Show a dash instead of 0% when session hasn't started yet
    five_label = f"<b>{five_pct}%</b>" if five.get("resets_at") else "<b>--</b>"
    seven_label = f"<b>{seven_pct}%</b>" if seven.get("resets_at") else "<b>--</b>"

    text = (
        f"<span color='#cba6f7'>󰚩</span> "
        f"<span color='#bac2de'>5h</span> "
        f"<span color='{five_color}'>{five_label}</span>  "
        f"<span color='#bac2de'>7d</span> "
        f"<span color='{seven_color}'>{seven_label}</span>"
    )

    # --- TOOLTIP ---
    tt = "<b><span color='#cba6f7'>╭───────────────────────────────╮</span></b>\n"

    tt += f"<b><span color='#cba6f7'>│</span></b> <b><span color='#89dceb'>SESSION</span></b> <span color='#585b70'>(5h)</span>  <span color='#9399b2'>resets in {five_reset}</span>\n"
    tt += f"<b><span color='#cba6f7'>│</span></b> <span color='#45475a'>[{get_progress_bar(five_pct)}]</span> <span color='{five_color}'>{five_label}</span>\n"

    tt += "<b><span color='#cba6f7'>├───────────────────────────────┤</span></b>\n"

    tt += f"<b><span color='#cba6f7'>│</span></b> <b><span color='#89b4fa'>WEEKLY</span></b> <span color='#585b70'>(7d)</span>  <span color='#9399b2'>resets in {seven_reset}</span>\n"
    tt += f"<b><span color='#cba6f7'>│</span></b> <span color='#45475a'>[{get_progress_bar(seven_pct)}]</span> <span color='{seven_color}'>{seven_label}</span>\n"

    if extra.get("is_enabled"):
        extra_pct = round(float(extra.get("utilization") or 0))
        extra_color = usage_color(extra_pct)
        used = extra.get("used_credits", 0) / 100  # cents → dollars
        limit = extra.get("monthly_limit", 0) / 100
        tt += "<b><span color='#cba6f7'>├───────────────────────────────┤</span></b>\n"
        tt += "<b><span color='#cba6f7'>│</span></b> <b><span color='#fab387'>EXTRA (pay-as-you-go)</span></b>\n"
        tt += f"<b><span color='#cba6f7'>│</span></b> <span color='#45475a'>[{get_progress_bar(extra_pct)}]</span> <span color='{extra_color}'><b>{extra_pct}%</b></span>  <span color='#9399b2'>${used:.2f}/${limit:.2f}</span>\n"

    tt += "<b><span color='#cba6f7'>╰───────────────────────────────╯</span></b>"

    return text, tt


def main():
    token, err = get_token()
    if not token:
        output = {
            "text": "󰚩 --",
            "tooltip": f"<b><span color='#f38ba8'>Authentication failed</span></b>\n{err}",
        }
        sys.stdout.write(json.dumps(output) + "\n")
        sys.stdout.flush()
        return

    data, err = fetch_usage(token)
    if not data:
        output = {
            "text": "󰚩 --",
            "tooltip": f"<b><span color='#f38ba8'>Failed to fetch usage</span></b>\n{err}",
        }
    else:
        text, tt = build_output(data)
        output = {"text": text, "tooltip": tt}

    sys.stdout.write(json.dumps(output) + "\n")
    sys.stdout.flush()


if __name__ == "__main__":
    main()

