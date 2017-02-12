#!/bin/bash

function run_single_test {
    ./bin/varan ${@} -o .test
    ./.test > /dev/null
    rm .test 
}

# Single files directly in test folder
for t in ./test/*.vr; do
    run_single_test $t
done

# Subdirectories consisting of multiple source files
for t in ./test/*/; do
    run_single_test `find $t/*.vr`
done
