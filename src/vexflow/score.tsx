import { useRef, useEffect } from "react";
import VexFlow from "vexflow";

// copied and amended from https://github.com/markacola/react-vexflow

const VF = VexFlow.Flow;
const { Formatter, Stave, StaveNote } = VF;

const clefWidth = 30;
const timeWidth = 30;

export function Score({
  staves = [],
  clef = "treble",
  timeSignature = "4/4",
  width = 450,
  height = 150,
}: {
  staves: any[][];
  clef?: string;
  timeSignature?: string;
  width?: number;
  height?: number;
}) {
  const container = useRef<null | HTMLDivElement>(null);
  const rendererRef = useRef<any>(null);

  useEffect(() => {
    if (container.current != null) {
      rendererRef.current =
        rendererRef.current ||
        new VF.Renderer(container.current, VF.Renderer.Backends.SVG);

      const rendererC = rendererRef.current;
      rendererC.resize(width, height);
      const context = rendererC.getContext();
      context.setFont("Arial", 10, "").setBackgroundFillStyle("#eed");

      const clefAndTimeWidth =
        (clef ? clefWidth : 0) + (timeSignature ? timeWidth : 0);
      const staveWidth = (width - clefAndTimeWidth) / staves.length;

      let currX = 0;
      staves.forEach((notes: any[], i: number) => {
        const stave = new Stave(currX, 0, staveWidth);
        if (i === 0) {
          stave.setWidth(staveWidth + clefAndTimeWidth);
          clef && stave.addClef(clef);
          timeSignature && stave.addTimeSignature(timeSignature);
        }
        currX += stave.getWidth();
        stave.setContext(context).draw();

        const processedNotes = notes
          .map((note: any) => (typeof note === "string" ? { key: note } : note))
          .map((note: any) =>
            Array.isArray(note) ? { key: note[0], duration: note[1] } : note,
          )
          .map(({ key, ...rest }: any) =>
            typeof key === "string"
              ? {
                  key: key.includes("/") ? key : `${key[0]}/${key.slice(1)}`,
                  ...rest,
                }
              : rest,
          )
          .map(
            ({ key, keys, duration = "q" }: any) =>
              new StaveNote({
                keys: key ? [key] : keys,
                duration: String(duration),
              }),
          );
        Formatter.FormatAndDraw(context, stave, processedNotes);
      });
    }
  }, [staves, clef, height, width, timeSignature, container]);

  return <div ref={container} />;
}
