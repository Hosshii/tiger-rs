export type WorkerInput = {
  type: "run wasm";
  payload: {
    src: string; // tiger source code
    stdin: string;
  };
};

export type WorkerOutput = {
  type: "run wasm";
  payload: {
    stdout: string;
    exitCode: number;
  };
};
