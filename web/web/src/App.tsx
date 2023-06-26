import { useEffect, useRef, useState } from "react";
import { Editor } from "./components/Editor";
import "./App.css";
import TextArea from "./components/Stdio";
import { ButtonLink, Button } from "./components/ButtonLink";
import { WorkerOutput, WorkerInput } from "./wasm/types";

const initTigerProg = `let
  var a := getchar()
in
  print(a)
end
`;

function App() {
  const [srcCode, setSrcCode] = useState("");
  const [result, setResult] = useState("");
  const [stdin, setStdin] = useState("hello world!");
  const workerRef = useRef<Worker | null>(null);
  const handleCodeChange = (s: string) => {
    setSrcCode(s);
  };
  const handleStdinChange = (s: string) => {
    setStdin(s);
  };

  const onClickRun = () => {
    workerRef.current?.postMessage({
      type: "run wasm",
      payload: { src: srcCode, stdin },
    } as WorkerInput);
  };

  useEffect(() => {
    workerRef.current = new Worker(
      new URL("./wasm/worker.ts", import.meta.url),
      { type: "module" }
    );
    workerRef.current.onmessage = (e: MessageEvent<WorkerOutput>) => {
      const data = e.data;
      if (data.type === "run wasm") {
        const { exitCode, stdout } = data.payload;
        const a = `exit code: ${exitCode}\n\nstdout:\n${stdout}`;
        setResult(a);
      }
    };

    return () => {
      workerRef.current && workerRef.current.terminate();
    };
  }, []);

  return (
    <>
      <div className="wrapper">
        <div className="grid-container">
          <div className="button-wrapper">
            <Button onClick={onClickRun}>Run</Button>
            <ButtonLink href="https://github.com/Hosshii/tiger-rs/tree/main/compiler-lib/tests/testfiles">
              Examples
            </ButtonLink>
          </div>
          <div className="editor-wrapper">
            <Editor onChange={handleCodeChange} initialValue={initTigerProg} />
          </div>
          <div className="stdin-title">In</div>
          <div className="stdin-wrapper">
            <TextArea
              content={stdin}
              readonly={false}
              onContentChange={handleStdinChange}
            />
          </div>
          <div className="stdout-title">Out</div>
          <div className="stdout-wrapper">
            <TextArea content={result} readonly={true} />
          </div>
        </div>
      </div>
    </>
  );
}

export default App;
