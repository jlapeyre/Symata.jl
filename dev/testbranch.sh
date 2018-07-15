#!/bin/bash

## This script makes a copy of the Symata source and runs the test suite.
## This script must be run from the ./dev directory.
## To use this script, you must have "./" first in LOAD_PATH,
## so that Julia finds the copy of the Symata package first.

## Get the first 9 characters of the HEAD commit
cd ../ && COMMIT_HASH=$(git rev-parse --short=9 HEAD) && cd ./dev/

copycode=false

## Make a commit-unique test directory
TEST_PACKAGE_DIR=./testsrc/Symata-${COMMIT_HASH}
if [ ! -d $TEST_PACKAGE_DIR/ ]; then
    mkdir $TEST_PACKAGE_DIR/
    copycode=true
fi


if [ "$copycode" = true ]; then
    ## Clean the test directory if it exists and add a package directory, Symata
    rm -rf $TEST_PACKAGE_DIR/*
    mkdir $TEST_PACKAGE_DIR/Symata/
    ## Copy the source files to the test package directory
    ## Project.toml must be present (15 Jul 2018) for loading modules correctly.
    cp -a ../src ../sjtest ../pysrc ../symsrc ../Project.toml $TEST_PACKAGE_DIR/Symata/
fi

cd $TEST_PACKAGE_DIR/

# Important to not use "-i" here.
julia -e '@time using Symata; @sym(Tests())' > >(tee -a stdout.log) 2> >(tee -a stderr.log >&2)

## This buffers all output till the end.
#julia -e '@time using Symata; @sym(Tests())' &> tests.log


# exit 1
