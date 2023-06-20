import React from "react";
import ReactDOM from "react-dom/client";
import "./index.css";
import { Editor } from "./components/Editor.tsx";

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>
    <Editor />
  </React.StrictMode>
);
