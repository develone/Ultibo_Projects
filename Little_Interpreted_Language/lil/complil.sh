#!/usr/bin/env bash
#
# This is a script to compare the output of all *.lil files in the
# current directory using two different LIL executables, which can be
# used to ensure consistency between LIL versions and implementations
#
# Usage: complil.sh lil1 lil2 [files to ignore]
#

LIL1=$1
LIL2=$2
IGNORE=${@:3}

if [[ "$LIL1" == "" ]]; then
    echo Missing first lil executable
    exit
fi
if [[ "$LIL2" == "" ]]; then
    echo Missing second lil executable
    exit
fi

for f in *.lil; do
    found=0
    for i in $IGNORE; do
        if [[ "$f" == "$i" ]]; then
            found=1
        fi
    done
    if [[ $found == 1 ]]; then
        continue
    fi
    echo Comparing $f
    rm -f $f.result1 $f.result2
    $LIL1 $f > $f.result1
    $LIL2 $f > $f.result2
    if ! cmp $f.result1 $f.result2 > /dev/null 2>&1; then
        echo Different results for $f
    else
        rm -f $f.result1 $f.result2
    fi
done
