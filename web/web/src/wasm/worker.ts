import { runWasm } from "./runWasm";
import { WorkerInput, WorkerOutput } from "./types";

self.onmessage = async (event: MessageEvent<WorkerInput>) => {
  const data = event.data;
  if (data.type == "run wasm") {
    const result = await runWasm(data.payload.src, data.payload.stdin);
    const output: WorkerOutput = {
      type: "run wasm",
      payload: {
        stdout: result.stdout,
        exitCode: result.exitCode,
      },
    };
    self.postMessage(output);
  }
};
