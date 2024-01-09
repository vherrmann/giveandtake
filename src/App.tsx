import { PitchDetector } from 'pitchy';
import { useEffect, useState } from 'react';
import './App.css';

type Nullable<T> = T | null | undefined;

// Define a function to fmap over a nullable value
function mapNullable<T, U>(value: Nullable<T>, mapper: (val: T) => U): Nullable<U> {
  switch (value) {
    case null:
      return null;
    case undefined:
      return undefined;
    default:
      return mapper(value);
  }
}

function CurrentPitchComp({analyserNode, detector, sampleRate}
                        : {analyserNode: AnalyserNode
                         , detector: Nullable<PitchDetector<Float32Array>>
                         , sampleRate: number}) {
  const [_time, setTime] = useState(Date.now());

  // rerender every 0.1s
  useEffect(() => {
    const interval = setInterval(() => setTime(Date.now()), 100);
    return () => {
      clearInterval(interval);
    };
  }, []);

  const [pitch, clarity] = (() => {
    switch(detector) {
      case null:
        return [null, null];
      case undefined:
        return [undefined, undefined];
      default:
        const input = new Float32Array(detector.inputLength);

        analyserNode.getFloatTimeDomainData(input);
        return detector.findPitch(input, sampleRate);
  }})();

  return (
    <>
      <div>
        <span id="pitch-label" className="label">Pitch</span>
        <span id="pitch" aria-labelledby="pitch-label">{pitch ? Math.floor(pitch*100)/100 : 'NaN'}</span>
      </div>
      <div>
        <span id="clarity-label" className="label">Clarity</span>
        <span id="clarity" aria-labelledby="clarity-label">{clarity ? Math.round(clarity * 100) : 'NaN'}</span>
      </div>
    </>
  );

}

function App() {
  const audioContext = new window.AudioContext();
  const analyserNode = audioContext.createAnalyser();
  const [streamN, setStreamN] = useState<Nullable<MediaStream>>(null);

  useEffect(() => {
    async function getStream() {
      const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
      setStreamN(stream);
    }
    getStream();
  }, [])

  const detector = mapNullable(streamN, (stream) => {
    audioContext.createMediaStreamSource(stream).connect(analyserNode);
    const detector = PitchDetector.forFloat32Array(analyserNode.fftSize);
    return detector;
  })

  return (
    <div className="App">
      <h1>Pitchy example</h1>
      <CurrentPitchComp
        analyserNode={analyserNode}
        detector={detector}
        sampleRate={audioContext.sampleRate}
      />
      <div>
        <button id="resume-button">Resume audio context</button>
      </div>

      <div id="vexflow-container"></div>

      <div id="progress-container">
        <div id="progress-bar"></div>
      </div>

      <span id="progress-label" className="progress">Progress</span>
      <span id="progress" aria-labelledby="progress-label"></span>

      <span id="score-label" className="score">Score</span>
      <span id="score" aria-labelledby="score-label"></span>
    </div>
  );
}

export default App;
