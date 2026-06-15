
hl.window_rule({
    name = "appblocker",
    match = { class = "^(appblocker-overlay)$" },
    float = true,
    no_anim = true,
    no_focus = true,
    pin = true,
    border_size = 0,
    opacity = "1.0 override 1.0 override",
})
