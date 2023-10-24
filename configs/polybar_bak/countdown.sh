#!/usr/bin/env bash

deadline=`date -d "Feb 3 2021" +%s`
today=`date +%s`
days=$(((($deadline - $today)/(3600*24))))
echo "Leads Finfactory:" $days "days left+1w";
