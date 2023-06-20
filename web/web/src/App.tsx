import { useState } from "react";
import { Editor } from "./components/Editor";
import "./App.css";

function App() {
  const [value, setValue] = useState("");
  const handleChange = (s: string) => {
    console.log(s);
    setValue(s);
  };

  return (
    <>
      <div className="wrapper">
        <div className="flex-container">
          <div>
            <button>Run</button>
          </div>
          <div className="editor-wrapper">
            <Editor value={value} onChange={handleChange} />
          </div>
        </div>
      </div>
    </>
  );
}

export default App;
