#!/bin/bash

# set -eux
set -u

assert() {
    local expected="$1"
    local file="./bin_test/$2"

    local bin="../target/release/tiger"

    $bin "$file" > tmp.s
    cc -o tmp tmp.s 
    ./tmp
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$2: success!!"
    else
        echo "$file => $expected expected, but got $actual"
        exit 1
    fi
}

cargo build --release
assert 1 test1.tig
assert 10 test2.tig
assert 10 test3.tig
assert 20 test4.tig
assert 10 test5.tig
assert 55 test6.tig
assert 1 test7.tig
assert 120 test8.tig
assert 2 test9.tig
assert 3 test10.tig