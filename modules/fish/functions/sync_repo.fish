function sync_repo -a path
    cd $path;
    git_update;
    git submodule foreach "(git remote get-url origin | grep p2d0) && (git add .;git commit -m 'update $(date "+%H:%M %a, %d %b")';git push origin)";
    cd -;
end
