# Rust implementation of the first part of "Modern Compiler Implementation in ML"

## Build
Only ARM64 macOS is supported.

### Build compiler.
```
cargo build --release
```

### Build tiger source code
1. Compile tiger source code.
```
./target/release/tiger <src file path> > tiger.s
```

2. Link it with runtime.
```
gcc tiger.s ./target/release/libcdylib.dylib
```

### Supported platform
- aarch64-apple-darwin

### Run test.
#### Unit test
``` 
cargo test
```

#### Integration test
```
cd compiler
./bin_test/test.sh
```