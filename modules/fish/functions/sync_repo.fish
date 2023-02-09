function sync_repo -a path
    cd $path;
    git_update;
    git submodule foreach "git add .;git commit -m 'update $(date "+%H:%M %a, %d %b")';git push origin";
    cd -;
end
