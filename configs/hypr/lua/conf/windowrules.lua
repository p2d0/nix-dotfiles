
-- Suppress maximize events for all windows
hl.window_rule({ match = { class = ".*" }, suppress_event = "maximize" })

-- KDenlive min size
hl.window_rule({ match = { class = "org.kde.kdenlive", float = true }, min_size = { 800, 600 } })

-- Float JPEG files
hl.window_rule({ match = { class = "file-jpeg" }, float = true })

-- Library window
hl.window_rule({ match = { title = "Library" }, float = true, center = true, opacity = 0.8, size = { 1160, 740 } })

-- Alacritty special workspace
hl.window_rule({ match = { class = "^(alacritty-sp)$" }, workspace = "special:alacritty" })

-- Choose dialog
hl.window_rule({ match = { title = "Choose.+" }, float = true, size = { 1160, 740 } })

-- Blueman manager
hl.window_rule({ match = { class = ".blueman-manager-wrapped" }, float = true, size = { 640, 640 }, move = "((monitor_w-window_w)-10) ((monitor_h-window_h)-10)" })

-- Fix XWayland dragging issues
hl.window_rule({ match = { class = "^$", title = "^$", xwayland = true, float = true, fullscreen = false, pin = false }, no_focus = true })

-- Brave tile
hl.window_rule({ match = { class = "Brave-browser" }, tile = true })

-- Guake
hl.window_rule({ match = { class = "guake" }, float = true, move = "(0) (0)", size = "(monitor_w*1) (monitor_h*0.4)" })

-- Firefox no border
hl.window_rule({ match = { class = "^(firefox)$" }, border_size = 0 })

-- osu! rules
hl.window_rule({ match = { class = "^(osu!)$", float = true }, opacity = 1 })
hl.window_rule({ match = { class = "^(osu!)$" }, no_blur = true })

-- darkwindow plugin config
-- hl.config({
--     plugin = {
--         darkwindow = {
--             shader = {
--                 tintRed = {
--                     introduces_transparency = false,
--                     from = "chromakey",
--                     args = "bkg=[0 0 0] amount=0 targetOpacity=0 similarity=0",
--                 },
--             },
--         },
--     },
-- })

-- Workspace w[t1] no border
hl.window_rule({ match = { workspace = "w[t1]" }, border_size = 0 })

-- Immediate rendering for games
hl.window_rule({ match = { class = "McEngine" }, immediate = true })
hl.window_rule({ match = { class = "osu!" }, immediate = true })
hl.window_rule({ match = { class = "org.vinegarhq.Sober" }, immediate = true })
hl.window_rule({ match = { class = "com.obsproject.Studio" }, immediate = true })
hl.window_rule({ match = { class = "steam_app_2642680" }, immediate = true })
hl.window_rule({ match = { class = "steam_app_default" }, immediate = true })
hl.window_rule({ match = { class = "steam" }, float = true })

-- Chatterino
hl.window_rule({ match = { class = "com.chatterino." }, float = true, border_size = 0 })

-- Workspace assignments
hl.window_rule({ match = { class = "brave-calendar.google.com__-Default" }, workspace = "10", max_size = { 200, 200 } })
hl.window_rule({ match = { class = "org.telegram.desktop" }, workspace = "11" })
hl.window_rule({ match = { class = "max" }, workspace = "12" })
hl.window_rule({ match = { class = "max" }, fullscreen_state = "0 1" })

-- Pavucontrol
hl.window_rule({ match = { class = "org.pulseaudio.pavucontrol" }, float = true, size = { 990, 610 }, move = "((monitor_w-window_w)-10) ((monitor_h-window_h)-10)" })

-- Flameshot named block
hl.window_rule({
    name = "flameshot-rules",
    match = { initial_title = "flameshot" },
    float = true,
    size = { 4480, 1080 },
    move = "0 0",
    pin = true,
    border_size = 0,
    stay_focused = true,
    opaque = true,
})

-- Picture-in-Picture
hl.window_rule({
    name = "pictureinpicture",
    match = { initial_title = "Picture-in-Picture" },
    size = { 1280, 720 },
    pin = true,
    monitor = monitor_1,
    move = "((monitor_w-1280)) 0",
    no_focus = true,
    opacity = 0.85,
    float = true,
})

-- Media viewer
hl.window_rule({ match = { title = "Media viewer" }, animation = "fade 1 1 linear", float = true, fullscreen_state = "1 1", border_size = 0 })
hl.window_rule({ match = { title = "Friends List" }, min_size = { 400, 700 } })
hl.window_rule({ match = { class = "xdg-desktop-portal-gtk" }, float = true })

-- Anki
hl.window_rule({ match = { class = "anki" }, float = true, size = { 900, 700 }, center = true })

-- Float rules for various titles
hl.window_rule({ match = { title = "OneTrainer" }, float = true })
hl.window_rule({ match = { title = "Concept" }, float = true })
hl.window_rule({ match = { title = "Sample" }, float = true })

-- xwaylandvideobridge
hl.window_rule({ match = { class = "^(xwaylandvideobridge)$" }, opacity = "0.0 override", no_anim = true, no_initial_focus = true, max_size = { 1, 1 }, no_blur = true, no_focus = true })
