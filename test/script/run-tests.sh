#!/bin/sh

args="$@"

dir=`dirname "$0"`
dir=`realpath "$dir"`

cd "$dir"

tests=`ls *.yu`

mini_yu="../../yuc"

run_test() {
  echo "TEST $1"
  "$mini_yu" "$1" $args
}

for t in $tests; do
  run_test "$t"
done
