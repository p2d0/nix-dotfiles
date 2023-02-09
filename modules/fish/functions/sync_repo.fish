function sync_repo -a path
    cd $path;
    git_update;
    git submodule foreach 'cd $toplevel/$path;fish -c "git_update";cd -;';
    cd -;
end
