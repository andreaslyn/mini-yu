#/bin/bash -e

root=`dirname "$0"`
root=`readlink -f "$root"`

cd "$root"

if ! git -C gcc branch 2>&1 | grep -q "yu-stack"; then
  git clone https://github.com/andreaslyn/gcc.git -b yu-stack
fi

mkdir -p gcc/yu-stack-install
mkdir -p gcc/yu-stack-build

cd gcc/yu-stack-build
../configure --enable-languages=c --prefix="$root"/gcc/yu-stack-install
