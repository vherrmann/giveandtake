import { useEffect, useRef } from "react";
import VexFlow from "vexflow";

const VF = VexFlow.Flow;

/* export function Chord({
 *   chordNotes = [],
 *   clef = "treble",
 *   timeSignature = "4/4",
 *   width,
 *   height,
 * }: {
 *   chordNotes: string[];
 *   clef?: string;
 *   timeSignature?: string;
 *   width?: number;
 *   height?: number;
 * }) {
 *   const canvasRef = useRef<HTMLDivElement | null>(null);
 *
 *   useEffect(() => {
 *     const canvas = canvasRef.current;
 *     if (canvas) {
 *       // clear div
 *       while (canvas.childNodes.length > 0) {
 *         canvas.removeChild(canvas.childNodes[0]);
 *       }
 *
 *       // FIXME: https://github.com/0xfe/vexflow/issues/580
 *       const vf = new VF.Factory({
 *         renderer: { elementId: canvas, width: width, height: height },
 *       });
 *
 *       const score = vf.EasyScore();
 *       const system = vf.System();
 *
 *       const chord = new VF.StaveNote({
 *         clef: "treble",
 *         keys: ["C/4"],
 *         duration: "q", // Quarter note
 *       });
 *
 *       system
 *         .addStave({
 *           voices: [score.voice([chord], null)],
 *         })
 *         .addClef(clef)
 *         .addTimeSignature(timeSignature);
 *
 *       vf.draw();
 *     }
 *   }, [canvasRef, chordNotes, clef, timeSignature, width, height]);
 *
 *   return <div ref={canvasRef} style={{ textAlign: "center" }} />;
 * }
 *  */

export function Chord({
  chordNotes = [],
  clef = "treble",
  timeSignature,
  width = 300,
}: {
  chordNotes: string[];
  clef?: string;
  timeSignature?: string;
  width?: number;
}) {
  const canvasRef = useRef<HTMLDivElement | null>(null);

  useEffect(() => {
    const canvas = canvasRef.current;
    if (canvas !== null) {
      canvas.innerHTML = ""; // Clear previous content

      const renderer = new VF.Renderer(canvas, VF.Renderer.Backends.SVG);
      const context = renderer.getContext();
      const stave = new VF.Stave(0, 0, width);

      // Add a clef and time signature (optional)
      if (clef) stave.addClef(clef);
      if (timeSignature) stave.addTimeSignature(timeSignature);

      // Add the stave to the context
      stave.setContext(context).draw();

      // Create a new voice in 4/4 time
      const voice = new VF.Voice({ num_beats: 1, beat_value: 4 });

      // Add the root note to the notes array
      const chord = new VF.StaveNote({
        clef: "treble",
        keys: chordNotes,
        duration: "q", // Quarter note
      });

      voice.addTickables([chord]);

      VF.Accidental.applyAccidentals([voice]);

      // Format and draw the notes on the stave
      new VF.Formatter().joinVoices([voice]).format([voice], 300);

      voice.draw(context, stave);
    }
  }, [canvasRef, chordNotes, clef, timeSignature, width]);

  return <div ref={canvasRef} style={{ textAlign: "center" }} />;
}
