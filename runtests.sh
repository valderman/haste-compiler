#!/bin/bash

let failed=0
let tests=0

for file in Tests/*.hs; do
    let tests=$tests+1

    module=`echo $file | sed -e s/Tests\\\/// | sed -e s/\.hs//`

    echo "Running test $module..."

    ghc_output=`runghc -DTEST_MODULE=$module TestDriver.hs`

    hastec --start=asap -DHASTE -DTEST_MODULE=$module TestDriver.hs > /dev/null
    haste_output=`js TestDriver.js`

    hastec -O2 --start=asap -DHASTE -DTEST_MODULE=$module TestDriver.hs > /dev/null
    haste_opt_output=`js TestDriver.js`

    if [[ "$ghc_output" != "$haste_output" ]] ; then
        thistest="failed"
        echo "  GHC disagrees with hastec output!"
        echo "  GHC says '$ghc_output', but hastec says '$haste_output'"
    fi

    if [[ "$ghc_output" != "$haste_opt_output" ]] ; then
        thistest="failed"
        echo "  GHC disagrees with hastec -O2 output!"
        echo "  GHC says '$ghc_output', but hastec says '$haste_out_output'"
    fi

    if [[ $thistest == "failed" ]] ; then
        let failed=$failed+1
    fi
done

echo
let success=$tests-$failed
echo "$success/$tests succeeded"
