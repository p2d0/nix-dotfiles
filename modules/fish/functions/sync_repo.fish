function sync_repo -a path
    cd $path;
    git_update;
    git submodule foreach --recursive 'cd $toplevel/$path;fish -c "git_update";cd -;';
    cd -;
end
