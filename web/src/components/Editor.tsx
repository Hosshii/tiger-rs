import React, { useRef, useState, useEffect } from "react";
import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import styles from "./Editor.module.css";

export const Editor: React.FC = () => {
  const [editor, setEditor] =
    useState<monaco.editor.IStandaloneCodeEditor | null>(null);
  const monacoEl = useRef(null);

  useEffect(() => {
    if (monacoEl) {
      setEditor((editor) => {
        if (editor) return editor;

        return monaco.editor.create(monacoEl.current!, {
          value: ["function x() {", '\tconsole.log("Hello world!");', "}"].join(
            "\n"
          ),
          language: "typescript",
          lineNumbers: "off", // テキストエディタの行番号を表示するかどうか
          roundedSelection: true, // 選択範囲を角丸にするかどうか
          scrollBeyondLastLine: false, // テキストエディタの最後の行を超えてスクロールするかどうか
          readOnly: false, // テキストエディタを読み取り専用にするかどうか
          theme: "vs-dark", // テキストエディタのテーマ
          automaticLayout: true,
        });
      });
    }

    return () => editor?.dispose();
  }, [monacoEl.current]);

  return <div className={styles.Editor} ref={monacoEl}></div>;
};
