#!/bin/bash

runTest() {
    module=$1
    echo "Running test $module..."

    ghc_output=`runghc -DTEST_MODULE=$module TestDriver.hs`

    hastec --start=asap -DTEST_MODULE=$module TestDriver.hs > /dev/null 2>&1
    haste_output=`js TestDriver.js`

    hastec -O2 --start=asap -DO2 -DTEST_MODULE=$module TestDriver.hs > /dev/null 2>&1
    haste_opt_output=`js TestDriver.js`

    if [[ "$ghc_output" != "$haste_output" ]] ; then
        thistest="failed"
        echo "  GHC disagrees with hastec output!"
        echo "  GHC says '$ghc_output', but hastec says '$haste_output'"
    fi

    if [[ "$ghc_output" != "$haste_opt_output" ]] ; then
        thistest="failed"
        echo "  GHC disagrees with hastec -O2 output!"
        echo "  GHC says '$ghc_output', but hastec says '$haste_opt_output'"
    fi
}

if [[ "$1" != "" ]] ; then
    runTest $1
    exit 0
fi

let failed=0
let tests=0

for file in Tests/*.hs; do
    let tests=$tests+1
    thistest="success"

    module=`echo $file | sed -e s/Tests\\\/// | sed -e s/\.hs//`
    runTest $module

    if [[ $thistest == "failed" ]] ; then
        let failed=$failed+1
    fi
done

echo
let success=$tests-$failed
echo "$success/$tests succeeded"
