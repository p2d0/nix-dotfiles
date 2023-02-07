#!/usr/bin/env bash
DIR="$(cd "$(dirname "$0")" && pwd)"
choice=$(echo -e "\n\nﭮ\n\n" | dmenu -fn 'ShureTechMono Nerd Font-15' -i -p "New name for this workspace:" || exit 1)
i3-msg 'rename workspace to "'$("$DIR/get_workspace_number.sh")' : '$choice'"'
# WS=`python3 -c "import json; print(next(filter(lambda w: w['focused'], json.loads('$(i3-msg -t get_workspaces)')))['num'])"`; i3-input -F "rename workspace to $WS:%s" -P 'New name: '
