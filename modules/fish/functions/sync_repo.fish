function sync_repo -a path
    cd $path;
    git submodule foreach --recursive 'cd $toplevel/$path;fish -c "git_update";cd -;';
    git_update;
    cd -;
end
