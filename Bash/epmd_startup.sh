#!/bin/sh
if ! pgrep -x "epmd" > /dev/null 
then 
    epmd -daemon 
fi
