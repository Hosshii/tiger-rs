use anyhow::{Context as _, Result};
use rand::{distributions::Alphanumeric, thread_rng, Rng};
use rayon::prelude::*;
use std::{
    fs::{self, File},
    path::PathBuf,
    process::Command,
};
use tiger::{Arch, Compiler};

const TEST_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/", "tests");

const TEST_FILES: [(i32, &str); 48] = [
    (1, "test1.tig"),
    (10, "test2.tig"),
    (10, "test3.tig"),
    (20, "test4.tig"),
    (10, "test5.tig"),
    (55, "test6.tig"),
    (1, "test7.tig"),
    (120, "test8.tig"),
    (2, "test9.tig"),
    (3, "test10.tig"),
    (1, "test11.tig"),
    (2, "test12.tig"),
    (12, "test13.tig"),
    (20, "test14.tig"),
    (90, "test15.tig"),
    (0, "test16.tig"),
    (0, "test17.tig"),
    (0, "test18.tig"),
    //  0 test19.tig
    (1, "test20.tig"),
    (0, "test21.tig"),
    (0, "test22.tig"),
    (0, "test23.tig"),
    (1, "test24.tig"),
    (1, "test25.tig"),
    (1, "test26.tig"),
    (1, "test27.tig"),
    (0, "test28.tig"),
    (1, "test29.tig"),
    (0, "test30.tig"),
    (1, "test31.tig"),
    (0, "test32.tig"),
    (1, "test33.tig"),
    (97, "test34.tig"),
    (65, "test35.tig"),
    (48, "test36.tig"),
    (1, "test37.tig"),
    (97, "test38.tig"),
    (12, "test39.tig"),
    (1, "test40.tig"),
    (0, "test41.tig"),
    (1, "test42.tig"),
    //  1 test43.tig # in wasm, ignore this
    (3, "test44.tig"),
    (2, "test45.tig"),
    (0, "test46.tig"),
    (5, "test47.tig"),
    (10, "test48.tig"),
    (65, "test49.tig"),
    (31, "test50.tig"),
];

#[test]
fn test_wasm() {
    TEST_FILES.par_iter().for_each(|(expected, file_name)| {
        let tiger_file = PathBuf::from(TIGER_FILE_DIR).join(file_name);
        test_on_wasm(*expected, &tiger_file, JS_FILE).unwrap();
    });
}

#[cfg(any(
    all(target_arch = "x86_64", target_os = "linux"),
    all(target_arch = "x86_64", target_os = "macos"),
    all(target_arch = "aarch64", target_os = "macos"),
))]
#[test]
fn test_unixlike() {
    cfg_if::cfg_if! {
        if #[cfg(all(target_arch = "x86_64", target_os = "linux"))] {
            use tiger::X86_64LinuxGnu as ARCH;
        } else if #[cfg(all(target_arch = "x86_64", target_os = "macos"))] {
            use tiger::X86_64AppleDarwin as ARCH;
        } else if #[cfg(all(target_arch = "aarch64", target_os = "macos"))] {
            use tiger::Aarch64AppleDarwin as ARCH;
        }
    }

    TEST_FILES.par_iter().for_each(|(expected, file_name)| {
        let tiger_file = PathBuf::from(TIGER_FILE_DIR).join(file_name);
        test_on_unixlike::<ARCH>(*expected, &tiger_file, CDYLIB_FILE).unwrap();
    });
}

cfg_if::cfg_if! {
    if #[cfg(target_os = "linux")] {
        const CDYLIB_FILE: &str = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../target/release/libcdylib.so"
        );
    } else if #[cfg(target_os = "macos")] {
        const CDYLIB_FILE: &str = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../target/release/libcdylib.dylib"
        );
    }
}

fn test_on_unixlike<A: Arch>(
    expected: i32,
    tiger_file: &PathBuf,
    library_file: &str,
) -> Result<()> {
    let tiger_file_name = tiger_file
        .file_name()
        .context("tiger_file has no filename")?;
    let tiger_file_name = tiger_file_name.to_string_lossy();
    let tiger_file = File::open(tiger_file)
        .with_context(|| format!("failed to open tiger_file {}", tiger_file.display()))?;

    let asm_name = format!("{}.s", gen_rand_string(10));
    let asm_path = PathBuf::from(TEST_DIR).join(asm_name);
    let mut asm = File::create(&asm_path).context("cannot create file")?;

    Compiler::new::<A>(tiger_file_name, tiger_file, &mut asm).compile()?;

    let exe_name = format!("{}.out", gen_rand_string(10));
    let exe_path = PathBuf::from(TEST_DIR).join(exe_name);

    let mut clang = Command::new("clang")
        .arg("-o")
        .arg(exe_path.display().to_string())
        .arg("-no-pie")
        .arg(library_file)
        .arg(asm_path.display().to_string())
        .spawn()
        .context("cannot spawn clang")?;

    clang.wait().context("clang terminated with an error")?;

    let mut exe = Command::new(exe_path.display().to_string())
        .spawn()
        .with_context(|| format!("cannot execute exe {}", exe_path.display()))?;
    let status = exe.wait().context("exe terminated with an error")?;

    fs::remove_file(asm_path).context("cannot remove file")?;
    fs::remove_file(exe_path).context("cannot remove file")?;

    assert_eq!(expected, status.code().unwrap());
    Ok(())
}

const TIGER_FILE_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/testfiles");
const JS_FILE: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/test.js");

fn test_on_wasm(expected: i32, tiger_file: &PathBuf, js_file: &str) -> Result<()> {
    let name = tiger_file
        .file_name()
        .context("tiger_file has no filename")?;
    let name = name.to_string_lossy();
    let tiger_file = File::open(tiger_file)
        .with_context(|| format!("failed to open tiger_file {}", tiger_file.display()))?;

    let mut out_path = PathBuf::from(TEST_DIR).join(gen_rand_string(10));
    out_path.set_extension("wasm");

    let mut out = File::create(&out_path).context("cannot create file")?;
    Compiler::new::<tiger::Wasm32UnknownUnknown>(name, tiger_file, &mut out).compile()?;

    let mut cmd = Command::new("node")
        .arg(js_file)
        .arg(out_path.display().to_string())
        .arg(expected.to_string())
        .spawn()
        .context("cannot spawn clang")?;

    let status = cmd.wait().context("clang terminated with an error")?;

    fs::remove_file(out_path).context("cannot remove file")?;

    assert!(status.success());
    Ok(())
}

fn gen_rand_string(len: usize) -> String {
    thread_rng()
        .sample_iter(&Alphanumeric)
        .take(len)
        .map(char::from)
        .collect()
}
