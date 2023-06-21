import React, { useRef, useState, useEffect } from "react";
import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import "./Editor.css";

export const Editor: React.FC<{
  initialValue?: string;
  value?: string;
  onChange?: (text: string) => void;
  readOnly?: boolean;
  lineNumbers?: monaco.editor.LineNumbersType;
  renderLineHighlight?: "none" | "gutter" | "line" | "all";
}> = ({
  initialValue = "",
  value,
  onChange,
  readOnly = false,
  lineNumbers = "on",
  renderLineHighlight,
}) => {
  const [editor, setEditor] =
    useState<monaco.editor.IStandaloneCodeEditor | null>(null);
  const monacoEl = useRef(null);

  useEffect(() => {
    if (monacoEl) {
      setEditor((editor) => {
        if (editor) return editor;

        const newEditor = monaco.editor.create(monacoEl.current!, {
          value: initialValue,
          language: "",
          lineNumbers: lineNumbers, // テキストエディタの行番号を表示するかどうか
          roundedSelection: true, // 選択範囲を角丸にするかどうか
          scrollBeyondLastLine: false, // テキストエディタの最後の行を超えてスクロールするかどうか
          readOnly: readOnly, // テキストエディタを読み取り専用にするかどうか
          // theme: "vs-dark", // テキストエディタのテーマ
          automaticLayout: true,
          renderLineHighlight: renderLineHighlight,
          minimap: { enabled: false },
        });

        newEditor.getModel()?.onDidChangeContent(() => {
          onChange?.(newEditor.getValue());
        });

        return newEditor;
      });
    }

    return () => editor?.dispose();
  }, [monacoEl.current]);

  useEffect(() => {
    editor?.getModel()?.setValue(value ?? initialValue);
  }, [editor, value, initialValue]);

  return <div className="editor" ref={monacoEl}></div>;
};
