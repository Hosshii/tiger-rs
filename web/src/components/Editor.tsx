import React, { useRef, useState, useEffect } from "react";
import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import "./Editor.css";

export const Editor: React.FC<{
  value: string;
  onChange: (text: string) => void;
}> = ({ value, onChange }) => {
  const [editor, setEditor] =
    useState<monaco.editor.IStandaloneCodeEditor | null>(null);
  const monacoEl = useRef(null);

  useEffect(() => {
    if (monacoEl) {
      setEditor((editor) => {
        if (editor) return editor;

        const newEditor = monaco.editor.create(monacoEl.current!, {
          value: value,
          language: "",
          lineNumbers: "on", // テキストエディタの行番号を表示するかどうか
          roundedSelection: true, // 選択範囲を角丸にするかどうか
          scrollBeyondLastLine: false, // テキストエディタの最後の行を超えてスクロールするかどうか
          readOnly: false, // テキストエディタを読み取り専用にするかどうか
          // theme: "vs-dark", // テキストエディタのテーマ
          automaticLayout: true,
        });

        newEditor.getModel()?.onDidChangeContent(() => {
          onChange(newEditor.getValue());
        });

        return newEditor;
      });
    }

    return () => editor?.dispose();
  }, [monacoEl.current]);

  return <div className="editor" ref={monacoEl}></div>;
};
