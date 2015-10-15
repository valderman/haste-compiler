#!/bin/bash

hastec=hastec
OPTIMIZE="--opt-whole-program --opt-tail-chain-bound=20"

if [[ $JS == "" ]] ; then
    if [[ $(which nodejs) != "" ]] ; then
      JS=nodejs
    else
      JS=node
    fi
fi

if [[ -f 'cabal.sandbox.config' ]]; then
    SANDBOX_DIR=$(cat cabal.sandbox.config  | grep 'prefix:' | cut -d ':' -f 2)
    echo "Notice: running with a sandbox located at:$SANDBOX_DIR" >&2
fi

runTest() {
    module=$1
    quiet=$2
    haste_stderr_file=`mktemp`
    haste_opt_stderr_file=`mktemp`
    ghc_stderr_file=`mktemp`
    echo "Running test $module..."

    ghc_output=`cabal exec runghc -- -w -DTEST_MODULE=$module TestDriver.hs 2> $ghc_stderr_file`

    if [[ $quiet == 1 ]] ; then
        $hastec -fforce-recomp --onexec -O0 -DTEST_MODULE=$module TestDriver.hs > /dev/null 2>&1
    else
        $hastec -fforce-recomp -O0 --verbose --debug --onexec -DTEST_MODULE=$module TestDriver.hs
    fi
    haste_output=`$JS TestDriver.js 2> $haste_stderr_file`

    if [[ $quiet == 1 ]] ; then
        $hastec -fforce-recomp -DO2 $OPTIMIZE --onexec -DTEST_MODULE=$module --out=TestDriver.O2.js TestDriver.hs > /dev/null 2>&1
    else
        $hastec -fforce-recomp -DO2 $OPTIMIZE --verbose --debug --onexec -DTEST_MODULE=$module --out=TestDriver.O2.js TestDriver.hs
    fi
    haste_opt_output=`$JS TestDriver.O2.js 2> $haste_opt_stderr_file`

    haste_stderr=`cat $haste_stderr_file`
    haste_opt_stderr=`cat $haste_opt_stderr_file`
    ghc_stderr=`cat $ghc_stderr_file`
    rm $haste_stderr_file $haste_opt_stderr_file $ghc_stderr_file

    if [[ "$ghc_output" != "$haste_output" || "$ghc_stderr" != "$haste_stderr" ]] ; then
        thistest="failed"
        echo "  GHC disagrees with hastec output!"
        if [[ "$ghc_output" != "$haste_output" ]] ; then
            echo "  GHC says '$ghc_output', but hastec says '$haste_output'"
        else
            echo "  Mismatch on STDERR."
            echo "  GHC says '$ghc_stderr', but hastec says '$haste_stderr'"
        fi
    fi

    if [[ "$ghc_output" != "$haste_opt_output" || "$ghc_stderr" != "$haste_opt_stderr" ]] ; then
        thistest="failed"
        echo "  GHC disagrees with hastec -O2 output!"
        if [[ "$ghc_output" != "$haste_opt_output" ]] ; then
            echo "  GHC says '$ghc_output', but hastec says '$haste_opt_output'"
        else
            echo "  Mismatch on STDERR."
            echo "  GHC says '$ghc_stderr', but hastec says '$haste_opt_stderr'"
        fi
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
