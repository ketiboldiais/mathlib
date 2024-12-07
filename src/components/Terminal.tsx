"use client";

import { lexical, syntax, treestring } from "@/winnow/main";
import { KeyboardEventHandler, useState } from "react";

const Terminal = () => {
  const [input, setInput] = useState("");
  const [output, setOutput] = useState("");
  const handleInput = (txt: string) => {
    setInput(txt);
  };
  const handleTokenize = () => {
    const source = input;
    const result = lexical(source).stream();
    if (result.isLeft()) {
      setOutput(result.unwrap().toString());
    } else {
      setOutput(result.unwrap().toString());
    }
  };
  const handleParse = () => {
    const source = input;
    const result = syntax(source).parset();
    if (result.isLeft()) {
      setOutput(result.unwrap().toString());
    } else {
      setOutput(treestring(result.unwrap()));
    }
  };
  const handleKeyDown: KeyboardEventHandler<HTMLTextAreaElement> = (event) => {
    if (event.target instanceof HTMLTextAreaElement) {
      if (event.key === "Tab") {
        event.preventDefault();
        const start = event.target.selectionStart;
        const end = event.target.selectionEnd;
        const newvalue = input.substring(0, start) + '  ' + input.substring(end); 
        setInput(newvalue);
        event.target.selectionStart = event.target.selectionEnd = start + 2;
      }
    }
  };
  return (
    <div className="content-center border font-mono">
      <textarea
        value={input}
        onChange={(event) => handleInput(event.target.value)}
        className="border border-black-600"
        onKeyDown={handleKeyDown}
      />
      <div>
        <button onClick={() => handleTokenize()} className="border">
          Scan
        </button>
        <button onClick={() => handleParse()} className="border">
          Parse
        </button>
      </div>
      {output && <pre>{output}</pre>}
    </div>
  );
};

export default Terminal;
