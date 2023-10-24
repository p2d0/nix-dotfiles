#!/usr/bin/env bash


toggle(){
    read work < work.txt
    echo $work;
    if [[ $work = 1 ]]; then
        work=0;
        echo $work > work.txt;
    else
        work=1;
        echo $work > work.txt;
    fi
}

display(){
    read work < work.txt
    if [[ $work = 1 ]]; then
        echo "%{B#f00}            Working...          %{B-}";
    else
        echo "%{B#008000}            Resting...          %{B-}";
    fi
}

touch work.txt
case "$1" in
    --toggle)
        toggle
        ;;
    *)
        display
        ;;
esac
