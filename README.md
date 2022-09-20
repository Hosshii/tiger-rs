# Rust implementation of the first part of "Modern Compiler Implementation in ML"

## Build
Only ARM64 and x86_64 macOS is supported.

### Build compiler.
```
cargo build --release
```



### Build tiger source code

1. Compile tiger source code.
```
./target/release/tiger <src file path> > tiger.s
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

### Run test.
#### Unit test
``` 
cargo test
```

#### Integration test
Only ARM64 mac is supported.

```
cd compiler
./bin_test/test.sh
```