#!/bin/bash

# set -eux
set -u

script_dir=$(cd $(dirname $0) && pwd)
root_dir=$(dirname $(dirname $script_dir))

assert_arm() {
    local expected="$1"
    local file="${script_dir}/$2"

    local bin="${root_dir}/target/release/tiger"

    $bin "$file" > tmp.s
    cc -o tmp.out tmp.s ${root_dir}/target/release/libcdylib.dylib
    ./tmp.out
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$2: success!!"
    else
        echo "$file => $expected expected, but got $actual"
        exit 1
    fi
}

assert_wasm(){
    local expected="$1"
    local file="${script_dir}/$2"
    local tmp_wasm="${script_dir}/wasm/test.wasm"
    local tmp_wat="${script_dir}/wasm/test.wat" # for debug
    local test_js="${script_dir}/wasm/test.js"
    local rust_lib="${root_dir}/target/wasm32-unknown-unknown/release/cdylib.wasm"

    local bin="${root_dir}/target/release/tiger"

    $bin "$file" --arch wasm32-unknown-unknown > $tmp_wasm
    $bin "$file" --arch wasm32-unknown-unknown --wat > $tmp_wat
    node ${test_js} $tmp_wasm $rust_lib $expected
    if [ "$?" = "0" ]; then
        echo "$2: success!!"
    else
        echo "Test failed."
        exit 1
    fi
}

assert() {
    # assert_arm "$1" "$2"
    assert_wasm "$1" "$2"
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
assert 1 test11.tig
assert 2 test12.tig
assert 12 test13.tig
assert 20 test14.tig
assert 90 test15.tig
assert 0 test16.tig
assert 0 test17.tig
assert 0 test18.tig
# assert 0 test16.tig
assert 1 test20.tig
assert 0 test21.tig
assert 0 test22.tig
assert 0 test23.tig
assert 1 test24.tig
assert 1 test25.tig
assert 1 test26.tig
assert 1 test27.tig
assert 0 test28.tig
assert 1 test29.tig
assert 0 test30.tig
assert 1 test31.tig
assert 0 test32.tig
assert 1 test33.tig
assert 97 test34.tig
assert 65 test35.tig
assert 48 test36.tig
assert 1 test37.tig
assert 97 test38.tig
assert 12 test39.tig
assert 1 test40.tig
assert 0 test41.tig
assert 1 test42.tig
assert 1 test43.tig
assert 3 test44.tig

