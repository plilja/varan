#!/bin/bash

for t in ./test/*.vr; do
    ./bin/varan $t -o .test
    ./.test > /dev/null
    rm .test 
done

