#!/bin/bash

for i in $(seq -f "%05g" 1 1000)
do
    if [ -f ../out/reformated-simulated-observations-$i.json ]; then
        python timing.py ../out/reformated-simulated-observations-$i.json ../out/popsize-distribution-timing-$i.json ../app-config.json 
    fi
done


