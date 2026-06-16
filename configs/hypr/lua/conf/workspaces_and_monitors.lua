
-- Monitor definitions
hl.monitor({ output = monitor_1, mode = "2560x1080@74.99Hz", position = "0x0", scale = 1})
hl.monitor({ output = monitor_2, mode = "preferred", position = "2560x0", scale = 1 })
hl.monitor({ output = test, disabled = true })

-- Workspace rules (persistent)
for i = 1, 6 do
    hl.workspace_rule({ workspace = tostring(i), monitor = monitor_1, persistent = true })
end
for i = 7, 12 do
    hl.workspace_rule({ workspace = tostring(i), monitor = monitor_2, persistent = true })
end

hl.workspace_rule({ workspace = '1', default = true})
hl.workspace_rule({ workspace = '9', default = true})
hl.workspace_rule({ workspace = '11', layout = "lua:ff-tg" })
