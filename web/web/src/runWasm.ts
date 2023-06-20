import init_tiger, { compile } from "./pkg_compiler/web";
import init_cdylib, { read_stdout, write_stdin } from "./pkg_cdylib/cdylib";

async function runWasm(
  src: string,
  stdin: string
): Promise<{ exitCode: number; stdout: string }> {
  await init_tiger();
  let wasm;
  try {
    wasm = compile(src);
  } catch (e) {
    return { exitCode: 1, stdout: e };
  }

  const cdylib = await init_cdylib();

  const importObj = {
    env: cdylib,
  };

  const { instance } = await WebAssembly.instantiate(wasm, importObj);
  write_stdin(stdin);
  const exitCode = Number(instance.exports._start());

  const stdout = read_stdout();

  return { exitCode, stdout };
}

export { runWasm };
