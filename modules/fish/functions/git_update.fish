function git_update
    git add .;
    git commit -m "update $(date "+%H:%M %a, %d %b")";
    git push origin;
end
