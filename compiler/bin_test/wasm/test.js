const fs = require('fs');
const assert = require('assert');

const filename = process.argv[2];
const expected = parseInt( process.argv[3]);
const bytes = fs.readFileSync( `${__dirname}/${filename}`);

if (expected === undefined) {
  console.error("Argument not provided");
  process.exit(1);
}

(async () => {
  const { instance } = await WebAssembly.instantiate(bytes);
  let result = Number(instance.exports._start());
  assert.strictEqual(result, expected);
})();