#!/bin/bash

for t in ./test/*.vr; do
    ./bin/varan $t
    ./a.out > /dev/null
    rm a.out
done

