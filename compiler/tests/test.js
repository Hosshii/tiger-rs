const fs = require('fs');
const assert = require('assert');
const cdylib = require('./pkg');

const tiger_file = process.argv[2];
const tiger_bytes = fs.readFileSync( `${tiger_file}`);

const expected = parseInt( process.argv[3]);


if (expected === undefined) {
  console.error("Argument not provided");
  process.exit(1);
}

(async () => {
  const importObj = {
    env: cdylib.__wasm,
  }
  const { instance } = await WebAssembly.instantiate(tiger_bytes, importObj);
  let result = Number(instance.exports._start());
  assert.strictEqual(result, expected);
})();
