import { compile } from "./pkg/web.js";
import init from "./pkg/cdylib.wasm?init";

async function runWasm(src: string): Promise<number> {
  const wasm = compile(src);
  const from_rust = await init({});
  const importObj = {
    env: from_rust.exports,
  };
  const { instance } = await WebAssembly.instantiate(wasm, importObj);
  const result = Number(instance.exports._start());
  return result;
}

export { runWasm };
