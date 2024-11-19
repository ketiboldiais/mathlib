"use client";

import { lexical } from "@/winnow/main";
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
    setOutput(result.toString());
  };
  return (
    <div className="content-center border">
      <textarea
        onChange={(event) => handleInput(event.target.value)}
        className="border border-black-600"
      />
      <div>
        <button onClick={() => handleTokenize()} className="border">
          Tokenize
        </button>
      </div>
      {output && <p>{output}</p>}
    </div>
  );
};

export default Terminal;
