import { isLine2D, isPath, isText, plot2D } from "@/winnow/main";

const Fig = () => {
  const par = "xMidYMid meet";
  const f = plot2D("fn f(x) = sin(x)")
    .domain(-5, 5)
    .range(-5, 5)
    .dimensions(200, 200)
    .samples(800)
    .axis("xy")
    .xTicks(.1, 1)
    .yTicks(.1, 1)
    .plot()
    .end()

  return (
    <svg
      viewBox={`0 0 ${f.$dimensions[0]} ${f.$dimensions[1]}`}
      preserveAspectRatio={par}
    >
      {f.$children.map((child) => {
        if (isPath(child)) {
          return (
            <path
              fill={"none"}
              stroke={"red"}
              strokeWidth={0.5}
              d={child.toString()}
              key={child.$id}
            />
          );
        } else if (isLine2D(child)) {
          return (
            <line
              x1={child.$start.$x}
              y1={child.$start.$y}
              x2={child.$end.$x}
              y2={child.$end.$y}
              stroke={"black"}
              strokeWidth={".2"}
              key={child.$id}
            />
          );
        } else if (isText(child)) {
          return (
            <text
              x={child.$position.$x}
              y={child.$position.$y}
              key={child.$id}
              fill="grey"
              fontSize={'4px'}
              textAnchor={child.$textAnchor}
            >
              {child.$text}
            </text>
          );
        }
        return <></>;
      })}
    </svg>
  );
};

export default Fig;
