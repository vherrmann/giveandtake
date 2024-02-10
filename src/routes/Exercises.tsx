import { ExpandLess, ExpandMore, Pause, PlayArrow } from "@mui/icons-material";
import {
  Button,
  Collapse,
  Divider,
  IconButton,
  LinearProgress,
} from "@mui/material";
import { PitchDetector } from "pitchy";
import { MutableRefObject, useEffect, useRef, useState } from "react";
import * as Tone from "tone";
import Box from "@mui/material/Box";
import { chordToVexflow, mapNullable, Nullable, randomInRange } from "../utils";
import { Chord } from "../vexflow/chord";
import * as Tonal from "tonal";
import * as Math from "mathjs";
import { LineChart, SparkLineChart } from "@mui/x-charts";

function freqRatioToCent(ratio: number) {
  return Math.abs(1200 * Math.log2(ratio));
}

export function NoteProgress({
  analyserNode,
  detector,
  sampleRate,
  onFinished,
}: {
  analyserNode: AnalyserNode;
  detector: Nullable<PitchDetector<Float32Array>>;
  sampleRate: number;
  onFinished: (finalPitch: number) => void;
}) {
  const [time, setTime] = useState(Date.now());
  const pitch = useRef<number | null>(null);

  switch (detector) {
    case null:
    case undefined:
      pitch.current = null;
      break;
    default:
      const input = new Float32Array(detector.inputLength);

      analyserNode.getFloatTimeDomainData(input);
      pitch.current = detector.findPitch(input, sampleRate)[0];
  }

  const lastSwitchTime = useRef(Date.now());
  const lastSwitchPitch = useRef(0);
  const lastPitches = useRef<number[]>([]);

  if (pitch.current) {
    lastPitches.current.push(Math.log2(pitch.current));
    lastPitches.current = lastPitches.current.slice(
      lastPitches.current.length - 100,
    );
  }

  const progress = (() => {
    if (pitch.current === null || pitch.current === 0) {
      return 0;
    } else {
      const ratio = lastSwitchPitch.current / pitch.current;
      const offByCents = freqRatioToCent(ratio);
      if (offByCents > 30) {
        lastSwitchPitch.current = pitch.current;
        lastSwitchTime.current = time;
      }
      // note has too be sung for 2 seconds
      return Math.min(1, (time - lastSwitchTime.current) / 2000);
    }
  })();

  // rerender every 0.1s
  useEffect(() => {
    const interval = setInterval(() => {
      const nextTime = Date.now();
      setTime(nextTime);

      if (pitch.current && nextTime - lastSwitchTime.current >= 2000) {
        lastSwitchPitch.current = pitch.current;
        lastSwitchTime.current = nextTime;
        onFinished(Math.mean(lastPitches.current.map((x) => 2 ** x)));
      }
    }, 100);

    return () => {
      clearInterval(interval);
    };
  }, [onFinished]);

  return (
    <>
      <div>{pitch.current}</div>
      <div>{100 * progress}</div>
      <LinearProgress
        variant="determinate"
        value={100 * progress}
        sx={{
          "& .MuiLinearProgress-bar": {
            transition: "none",
          },
        }}
      />
      <Box sx={{ flexGrow: 1 }}>
        {/* https://react-chartjs-2.js.org/examples/line-chart/ */}
        {/* <LineChart
            series={[{ curve: "linear", data: lastPitchesC }]}
            height={200}
            /> */}
        <SparkLineChart
          data={lastPitches.current}
          height={100}
          disableAxisListener={true}
        />
      </Box>
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

  const [active, setActive] = useState(false);
  const [rootNote, setRootNote] = useState<string>("");
  const [finalPitch, setFinalPitch] = useState<number | null>(null);
  const synth = useRef<Tone.Synth | null>(null);

  if (!rootNote) {
    setRootNote(
      rootNote || Tonal.Note.fromMidi(24 + 3 * 12 + randomInRange(0, 12)),
    );
  }
  const chord = chordToVexflow(Tonal.Chord.getChord("maj", rootNote).notes);
  const rootNoteFreq = Tonal.Note.freq(rootNote) || NaN;

  const changeActiveState = () => {
    // await Tone.start();

    synth.current = synth.current || new Tone.Synth().toDestination();

    if (!active) {
      synth.current.triggerRelease();
      synth.current.triggerAttack(`${rootNoteFreq}`);
    } else {
      synth.current.triggerRelease();
      if (finalPitch) {
        synth.current.triggerAttack(`${(rootNoteFreq * 3) / 2}`);
      }
    }

    setActive(!active);
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
          <NoteProgress
            analyserNode={analyserNode}
            detector={detector}
            sampleRate={audioContext.sampleRate}
            onFinished={(x) => {
              changeActiveState();
              setFinalPitch(x);
            }}
          />
        </div>
        {finalPitch ? (
          <>
            <p>Final Pitch: {finalPitch}</p>
            <p>Ratio: {finalPitch / rootNoteFreq}</p>
            <p>
              Quint off by: {freqRatioToCent(finalPitch / (1.5 * rootNoteFreq))}{" "}
              Cents
            </p>
          </>
        ) : (
          <> </>
        )}

        {/* <div style={{ textAlign: "center" }}>
            <Divider />
            <Button
            onClick={() => {
            setOpen(!open);
            }}
            >
            Debug {open ? <ExpandLess /> : <ExpandMore />}
            </Button>
            <Collapse in={open}>
            <CurrentPitchComp pitch={pitch} clarity={pitchClarity} />
            <div>
            <button id="resume-button">Resume audio context</button>
            </div>
            </Collapse>
            </div> */}
      </div>
    </Box>
  );
}
