


const Fig = () => {
  const width = 200;
  const height = 200;
  const viewbox = `0 0 ${width} ${height}`;
  const paddingBottom = `${100 * (height / width)}%`;
  const boxcss = {
    display: "inline-block",
    position: "relative",
    width: "100%",
    paddingBottom,
    overflow: "hidden",
  } as const;
  const svgcss = {
    display: "inline-block",
    position: "absolute",
    top: "0",
    left: "0",
    right: "0",
    bottom: "0",
  } as const;
  const par = "xMidYMid meet";
  const shift = (x: number, y: number) => `translate(${x},${y})`;
  const _mx = 50;
  const _my = 50;

  return (
    <svg
      viewBox="0 0 100 100"
      preserveAspectRatio={par}
      xmlns="http://www.w3.org/2000/svg"
    >
      <circle cx="50" cy="50" r="2" />
    </svg>
    // <div style={boxcss}>
    //   <svg viewBox={viewbox} preserveAspectRatio={par} style={svgcss}>
    //     <circle cx={50} cy={50} radius={100}/>
    //     <g transform={shift(_mx / 2, _my / 2)}>
    //     </g>
    //   </svg>
    // </div>
  );
};

export default Fig;
