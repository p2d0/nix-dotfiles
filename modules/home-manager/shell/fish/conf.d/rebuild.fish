
function rebuild-work
    if [ "$USER" != "root" ]
        echo "This function requires root privileges."
    else
        nixos-rebuild switch --impure  --flake '/etc/nixos/.?submodules=1#mysystem' -j6 $argv
            /run/current-system/specialisation/work/activate
        end
end
function activate-specialisation-work
    /run/current-system/specialisation/work/activate
end

function rebuild-default
    if [ "$USER" != "root" ]
        echo "This function requires root privileges."
    else
        nixos-rebuild switch --impure  --flake '/etc/nixos/.?submodules=1#mysystem' -j6 $argv
            /run/current-system/specialisation/default/activate
        end
    end

function activate-specialisation-default
    sudo /run/current-system/specialisation/default/activate
end
