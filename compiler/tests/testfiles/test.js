const fs = require('fs');
const assert = require('assert');

const tiger_file = process.argv[2];
const tiger_bytes = fs.readFileSync( `${tiger_file}`);

const rust_lib_file = process.argv[3];
const rust_lib_bytes = fs.readFileSync( `${rust_lib_file}`);

const expected = parseInt( process.argv[4]);


if (expected === undefined) {
  console.error("Argument not provided");
  process.exit(1);
}


(async () => {
  const from_rust = await WebAssembly.instantiate(rust_lib_bytes);

  const importObj = {
    env: from_rust.instance.exports,
  }
  const { instance } = await WebAssembly.instantiate(tiger_bytes, importObj);
  let result = Number(instance.exports._start());
  assert.strictEqual(result, expected);
})();
