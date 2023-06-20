import { useState } from "react";
import { Editor } from "./components/Editor";
import "./App.css";
import { runWasm } from "./runWasm";

function App() {
  const [value, setValue] = useState("");
  const [result, setResult] = useState(0);
  const handleChange = (s: string) => {
    console.log(s);
    setValue(s);
  };

  const onClick = () => {
    runWasm(value).then((res) => {
      setResult(res);
    });
  };

  return (
    <>
      <div className="wrapper">
        <div className="flex-container">
          <div>
            <button onClick={onClick}>Run</button>
          </div>
          <div className="editor-wrapper">
            <Editor value={value} onChange={handleChange} />
          </div>
          <div>{result}</div>
        </div>
      </div>
    </>
  );
}

export default App;
