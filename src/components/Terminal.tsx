"use client";

import { lexical, syntax, treestring } from "@/winnow/main";
import { useState } from "react";

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
    const result = syntax(source).statements();
    if (result.isLeft()) {
      setOutput(result.unwrap().toString());
    } else {
      setOutput(treestring(result.unwrap()));
    }
  };
  return (
    <div className="content-center border font-mono">
      <textarea
        onChange={(event) => handleInput(event.target.value)}
        className="border border-black-600"
      />
      <div>
        <button onClick={() => handleTokenize()} className="border">
          Scan
        </button>
        <button onClick={() => handleParse()} className="border">
          Parse
        </button>
      </div>
      {output && 
      <pre>
        {output}
      </pre>}
    </div>
  );
};

export default Terminal;
