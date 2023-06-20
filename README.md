# Rust implementation of the first part of "Modern Compiler Implementation in ML"

## Build

### Build compiler.

```
cargo build --release
```

### Build tiger source code

1. Compile tiger source code.

```
./target/release/tiger /path/to/src > tiger.s
```

You can specify [target](#supported-platform) by adding `--arch <target>` argument.

ex. `./target/release/tiger main.tig --arch x86_64-apple-darwin`

2. Link it with runtime.

```
gcc tiger.s ./target/release/libcdylib.dylib
```

### Supported platform

- aarch64-apple-darwin
- x86_64-apple-darwin
- x86_64-linux-gnu
- wasm32-unknown-unknown

### Run test.

`clang`, `wasm-pack`, `node` is required.

```
cargo test
```
