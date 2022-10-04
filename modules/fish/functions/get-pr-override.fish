function get-pr-override -a PR_NO
    set HASH (curl -sL https://github.com/NixOS/nixpkgs/pull/{$PR_NO}.patch \
        | head -n 1 | grep -o -E -e "[0-9a-f]{40}")
    echo pr{$PR_NO} = "import (fetchTarball"
    echo "  \"\${nixpkgs-tars}$HASH.tar.gz\")"
    echo "    { config = config.nixpkgs.config; };"
end
