#!/usr/bin/env bash

pkill -9 beam

sleep 3

M="20"

if [[ $1 != "" ]] ; then
    M=$1
fi


for I in $(eval echo {01..$M}); do
    mkdir -p tmp/test$I
    run_erl -daemon tmp/test$I tmp/ "exec erl -connect_all false -sname test$I -s main start"

done
