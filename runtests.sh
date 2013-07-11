#!/bin/bash

if [[ $JS == "" ]] ; then
    if [[ $(which nodejs) != "" ]] ; then
      JS=nodejs
    else
      JS=node
    fi
fi

runTest() {
    module=$1
    quiet=$2
    echo "Running test $module..."

    ghc_output=`runghc -DTEST_MODULE=$module TestDriver.hs`

    if [[ $quiet == 1 ]] ; then
        hastec --start=asap -O0 -DTEST_MODULE=$module TestDriver.hs --out=TestDriver.tmp > /dev/null 2>&1
    else
        hastec -O0 --verbose --debug --start=asap -DTEST_MODULE=$module TestDriver.hs --out=TestDriver.tmp
    fi
    echo "if(typeof print == 'undefined') {print = console.log}" > TestDriver.js
    cat TestDriver.tmp >> TestDriver.js
    haste_output=`$JS TestDriver.js`

    if [[ $quiet == 1 ]] ; then
        hastec -O2 --start=asap -DO2 -DTEST_MODULE=$module TestDriver.hs --out=TestDriver.tmp > /dev/null 2>&1
    else
        hastec -O2 --verbose --debug --start=asap -DO2 -DTEST_MODULE=$module --out=TestDriver.tmp TestDriver.hs
    fi
    echo "if(typeof print == 'undefined') {print = console.log}" > TestDriver.O2.js
    cat TestDriver.tmp >> TestDriver.O2.js
    haste_opt_output=`$JS TestDriver.O2.js`

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
    runTest $1 0
    exit 0
fi

let failed=0
let tests=0

for file in Tests/*.hs; do
    let tests=$tests+1
    thistest="success"

    module=`echo $file | sed -e s/Tests\\\/// | sed -e s/\.hs//`
    runTest $module 1

    if [[ $thistest == "failed" ]] ; then
        let failed=$failed+1
    fi
done

echo
let success=$tests-$failed
echo "$success/$tests succeeded"
