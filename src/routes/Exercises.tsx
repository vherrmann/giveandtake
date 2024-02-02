import { ExpandLess, ExpandMore, Pause, PlayArrow } from "@mui/icons-material";
import {
  Button,
  Collapse,
  Divider,
  IconButton,
  LinearProgress,
} from "@mui/material";
import { PitchDetector } from "pitchy";
import { useEffect, useRef, useState } from "react";
import * as Tone from "tone";
import Box from "@mui/material/Box";
import { chordToVexflow, mapNullable, Nullable, randomInRange } from "../utils";
import { Chord } from "../vexflow/chord";
import * as Tonal from "tonal";

function CurrentPitchComp({
  analyserNode,
  detector,
  sampleRate,
}: {
  analyserNode: AnalyserNode;
  detector: Nullable<PitchDetector<Float32Array>>;
  sampleRate: number;
}) {
  const [_time, setTime] = useState(Date.now());

  // rerender every 0.1s
  useEffect(() => {
    const interval = setInterval(() => setTime(Date.now()), 100);
    return () => {
      clearInterval(interval);
    };
  }, []);

  const [pitch, clarity] = (() => {
    switch (detector) {
      case null:
        return [null, null];
      case undefined:
        return [undefined, undefined];
      default:
        const input = new Float32Array(detector.inputLength);

        analyserNode.getFloatTimeDomainData(input);
        return detector.findPitch(input, sampleRate);
    }
  })();

  return (
    <>
      <div>
        <span id="pitch-label" className="label">
          Pitch
        </span>
        <span id="pitch" aria-labelledby="pitch-label">
          {" "}
          {pitch ? Math.floor(pitch * 100) / 100 : "NaN"}
        </span>
      </div>
      <div>
        <span id="clarity-label" className="label">
          Clarity
        </span>
        <span id="clarity" aria-labelledby="clarity-label">
          {" "}
          {clarity ? Math.round(clarity * 100) : "NaN"}
        </span>
      </div>
    </>
  );
}

export function Exercise1() {
  // FIXME: Start audiocontext in hook
  const audioContext = new window.AudioContext();
  const analyserNode = audioContext.createAnalyser();
  const [streamN, setStreamN] = useState<Nullable<MediaStream>>(null);

  useEffect(() => {
    async function getStream() {
      const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
      setStreamN(stream);
    }
    getStream();
  }, []);

  const detector = mapNullable(streamN, (stream) => {
    audioContext.createMediaStreamSource(stream).connect(analyserNode);
    const detector = PitchDetector.forFloat32Array(analyserNode.fftSize);
    return detector;
  });

  const [open, setOpen] = useState(false);
  const [active, setActive] = useState(false);
  const [synth, setSynth] = useState<Tone.Synth | null>(null);
  const [rootNote, setRootNote] = useState<string>("");

  if (!rootNote)
    setRootNote(
      rootNote || Tonal.Note.fromMidi(24 + 3 * 12 + randomInRange(0, 12)),
    );
  const chord = chordToVexflow(Tonal.Chord.getChord("maj", rootNote).notes);

  const changeActiveState = async function () {
    await Tone.start();

    const nextSynth = synth || new Tone.Synth().toDestination();

    if (!active) {
      nextSynth.triggerAttack(rootNote);
    } else {
      nextSynth.triggerRelease();
    }

    setActive(!active);
    setSynth(nextSynth);
  };

  return (
    <Box
      style={{
        width: "100%",
        display: "flex",
        justifyContent: "center",
        flexDirection: "row",
      }}
    >
      <div style={{ width: "300px", textAlign: "center" }}>
        <div>
          <IconButton size="large" onClick={changeActiveState}>
            {active ? <Pause /> : <PlayArrow />}
          </IconButton>
          <Chord chordNotes={chord} />
          <LinearProgress variant="determinate" value={50} />
          {/* TODO: https://mui.com/x/react-charts/sparkline/ */}
        </div>

        <div style={{ textAlign: "center" }}>
          <Divider />
          <Button
            onClick={() => {
              setOpen(!open);
            }}
          >
            Debug {open ? <ExpandLess /> : <ExpandMore />}
          </Button>
          <Collapse in={open}>
            <CurrentPitchComp
              analyserNode={analyserNode}
              detector={detector}
              sampleRate={audioContext.sampleRate}
            />
            <div>
              <button id="resume-button">Resume audio context</button>
            </div>
          </Collapse>
        </div>
      </div>
    </Box>
  );
}
