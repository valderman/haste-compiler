#!/bin/bash

hastec=hastec

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
        $hastec --onexec -O0 -DTEST_MODULE=$module TestDriver.hs > /dev/null 2>&1
    else
        $hastec -O0 --verbose --debug --onexec -DTEST_MODULE=$module TestDriver.hs
    fi
    haste_output=`$JS TestDriver.js`

    if [[ $quiet == 1 ]] ; then
        $hastec --opt-whole-program --onexec -DO2 -DTEST_MODULE=$module --out=TestDriver.O2.js TestDriver.hs > /dev/null 2>&1
    else
        $hastec --opt-whole-program --verbose --debug --onexec -DO2 -DTEST_MODULE=$module --out=TestDriver.O2.js TestDriver.hs
    fi
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
