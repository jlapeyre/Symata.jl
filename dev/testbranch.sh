#!/bin/bash

## This script makes a copy of the Symata source and runs the test suite.
## This script must be run from the ./dev directory

## Note: to use this script, you must have "./" first in LOAD_PATH
## so that Julia finds the copy of the Symata package first

cd ../ && COMMIT_HASH=$(git rev-parse --short=9 HEAD) && cd ./dev/

TEST_PACKAGE_DIR=./testsrc/Symata-${COMMIT_HASH}

echo mkdir $TEST_PACKAGE_DIR/
mkdir $TEST_PACKAGE_DIR/

echo rm -rf $TEST_PACKAGE_DIR/*
rm -rf $TEST_PACKAGE_DIR/*

mkdir $TEST_PACKAGE_DIR/Symata/

# # Project.toml must be present (15 Jul 2018) for loading modules correctly.
cp -a ../src ../sjtest ../pysrc ../symsrc ../Project.toml $TEST_PACKAGE_DIR/Symata/

# cd dev/testsrc && pwd

#cd testsrc && julia -e "@time using Symata; @time(@sym(Tests()))"

exit 1
