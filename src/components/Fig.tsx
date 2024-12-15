import {
  svg2D,
  group2D,
  isGroup2D,
  Graphics2DObj,
  Group2D,
  cplot2D,
  Path,
  isPath,
} from "@/winnow/main";

const Fig = () => {
  const par = "xMidYMid meet";
  const d = svg2D([
    group2D([cplot2D("fn f(x) = sin(x)").domain(-10, 10).done()]),
    group2D([cplot2D("fn g(x) = cos(x)").domain(-10, 10).done()]),
  ])
    .dimensions(500, 500)
    .domain(-5, 5)
    .range(-5, 5)
    .done();

  return (
    <svg viewBox={`0 0 ${d.$width} ${d.$height}`} preserveAspectRatio={par}>
      <Fig2D elements={d.$children} />
    </svg>
  );
};

type Fig2DProps = { elements: Graphics2DObj[] };

type Path2DProps = { element: Path };

const Path2D = ({ element }: Path2DProps) => {
  return <path d={element.toString()} fill="none" stroke="red" />;
};

const Fig2D = ({ elements }: Fig2DProps) => {
  return (
    <>
      {elements.map((element) => {
        if (isGroup2D(element)) {
          return <G2D key={element.$id} element={element} />;
        }
        if (isPath(element)) {
          return <Path2D key={element.$id} element={element} />;
        }
      })}
    </>
  );
};

type G2DProps = { element: Group2D };
const G2D = ({ element }: G2DProps) => {
  return (
    <g>
      <Fig2D elements={element.$children} />
    </g>
  );
};

export default Fig;
