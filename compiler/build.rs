use std::env;

fn main() {
    // `TARGET`はコンパイルターゲットを指定する環境変数です
    let target = env::var("TARGET").unwrap();
    println!("cargo:rustc-env=TARGET_TRIPLE={}", target);
}
