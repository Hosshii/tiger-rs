# Rust implementation of the first part of "Modern Compiler Implementation in ML"

## Build

### Requirements

- Rust
  - wasm32-unknown-unknown target
- cargo-make
- node

### Build compiler.

```sh
cargo build
```

or

```sh
cargo make build
```

### Build tiger source code.

1. Compile tiger source code.

```sh
./target/release/tiger /path/to/tiger-src > tiger.s
```

You can specify [target](#supported-platform) by adding `--arch <target>` argument.

ex. `./target/release/tiger main.tig --arch x86_64-apple-darwin`

2. Link it with runtime.

#### Linux

```sh
clang -no-pie tiger.s ./target/release/libcdylib.so
```

#### Macos

```sh
clang -no-pie tiger.s ./target/release/libcdylib.dylib
```

#### Wasm

see ([compiler-lib/Makefile.toml](./compiler-lib/Makefile.toml), [index.html](./compiler-lib/tests/index.html), [test.js](./compiler-lib/tests/test.js)) or ([web/Makefile.toml](./web/Makefile.toml), [runWasm.ts](./web/web/src/runWasm.ts)).

### Supported platform.

- aarch64-apple-darwin
- x86_64-apple-darwin
- x86_64-linux-gnu
- wasm32-unknown-unknown

### Run test.

```sh
cargo make test
```
