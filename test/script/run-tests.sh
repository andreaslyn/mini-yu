#!/bin/sh

args="$@"

dir=`dirname "$0"`
dir=`realpath "$dir"`

cd "$dir"

tests=`ls *.yu`

mini_yu="../../yuc"

run_test() {
  echo "CLEAN TEST $1"
  "$mini_yu" "$1" --clean -p "$dir/yu0" $args
  echo "LAZY  TEST $1"
  "$mini_yu" "$1" -p "$dir/yu0" $args
}

for t in $tests; do
  run_test "$t"
done
