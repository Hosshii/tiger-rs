import React from "react";
import "./Stdio.css";

interface TextAreaProps {
  readonly?: boolean;
  cursorVisible?: boolean;
  content?: string;
  onContentChange?: (content: string) => void;
}

const TextArea: React.FC<TextAreaProps> = ({
  readonly = false,
  cursorVisible = true,
  content = "",
  onContentChange,
}) => {
  const handleOnChange = (event: React.ChangeEvent<HTMLTextAreaElement>) => {
    if (onContentChange) {
      onContentChange(event.target.value);
    }
  };

  return (
    <textarea
      readOnly={readonly}
      style={{ cursor: cursorVisible ? "text" : "default" }}
      className="stdio-area"
      value={content}
      onChange={handleOnChange}
      translate="no"
    />
  );
};

export default TextArea;
