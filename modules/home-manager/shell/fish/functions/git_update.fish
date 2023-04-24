function git_update
    if git remote get-url origin | grep 'p2d0';
        echo "commiting";
        git add .;
        git commit -m "update $(date "+%H:%M %a, %d %b")";
        git push origin;
    end
end
