import init_tiger, { compile } from "../pkg_compiler/web";
import init_cdylib, { read_stdout, write_stdin } from "../pkg_cdylib/cdylib";

async function runWasm(
  src: string,
  stdin: string
): Promise<{ exitCode: number; stdout: string }> {
  await init_tiger();
  let wasm;
  try {
    wasm = compile(src);
  } catch (e) {
    return { exitCode: 1, stdout: e as string };
  }

  // TODO
  const cdylib = (await init_cdylib()) as any as WebAssembly.ModuleImports;

  const importObj = {
    env: cdylib,
  };

  const { instance } = await WebAssembly.instantiate(wasm, importObj);
  write_stdin(stdin);

  if (instance.exports._start instanceof Function) {
    const exitCode = Number(instance.exports._start());
    const stdout = read_stdout();
    return { exitCode, stdout };
  } else {
    return Promise.reject("No _start function");
  }
}

export { runWasm };
