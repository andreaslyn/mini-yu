#!/bin/bash

set -e

dir=`dirname "$0"`

compile() {
  echo 'yuc -co' $@
  "$dir"/../../yuc -co $@
}

compile_ghc() {
  pushd "$1" >/dev/null
  echo ghc '-O3' "$1"/Main.hs
  ghc -O3 Main.hs
  popd >/dev/null
}

compile_ocaml() {
  pushd "$1" >/dev/null
  echo ocamlopt "$1"/Main.ml -o Main
  ocamlopt Main.ml -o Main
  popd >/dev/null
}

compile_all() {
  compile "$dir/bin-test.yu"
  compile "$dir/extern-test.yu" "$dir/extern-test.c"
  compile "$dir/extern-str.yu" "$dir/extern-str.c"
  compile "$dir/hello-world.yu"
  compile "$dir/lazy-test.yu"
  compile "$dir/list-map-to-even.yu"
  compile "$dir/nat-test.yu"
  compile "$dir/lazy-nat-test.yu"
  compile "$dir/rev-test.yu"
  compile "$dir/lazy-rev-test.yu"
  compile "$dir/rev-test-bin.yu"
  compile "$dir/thread-test.yu"
  compile "$dir/sort-test.yu"

  compile_ghc "$dir/HsNatTest"
  compile_ghc "$dir/HsRevTest"
  compile_ghc "$dir/HsSortTest"

  compile_ocaml "$dir/OCamlNatTest"
  compile_ocaml "$dir/OCamlRevTest"
  compile_ocaml "$dir/OCamlSortTest"
}

clean_ghc() {
  rm -f "$1"/Main "$1"/Main.hi "$1"/Main.o "$1"/Main.s
}

clean_ocaml() {
  rm -f "$1"/Main "$1"/a.out "$1"/Main.cmi "$1"/Main.cmx "$1"/Main.o "$1"/Main.s
}

clean_all() {
  rm -f "$dir"/*.yu.exe
  rm -f "$dir"/*.yu.c
  rm -f "$dir"/*.yu.s

  clean_ghc "$dir/"HsNatTest
  clean_ghc "$dir/"HsRevTest
  clean_ghc "$dir/"HsSortTest

  clean_ocaml "$dir/"OCamlNatTest
  clean_ocaml "$dir/"OCamlRevTest
  clean_ocaml "$dir/"OCamlSortTest
}

if [ "$1" = compile ]; then
  compile_all
elif [ "$1" = clean ]; then
  clean_all
else
  echo expected subcommand: compile\|clean
fi
