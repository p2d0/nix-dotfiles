function sway-speedcrunch
    if swaymsg -t get_tree | jq '.nodes[].nodes[] | select(.name == "__i3_scratch").floating_nodes[] | select(.app_id == "org.speedcrunch.")' | jq 'length > 0' | grep -q true
        swaymsg "[app_id="org.speedcrunch."] scratchpad show, move to output current, move position 100ppt 30ppt, move left 300px"
    else
        swaymsg "[app_id="org.speedcrunch."] scratchpad show"
    end
    # swaymsg "[app_id="org.speedcrunch."] scratchpad show"
end
