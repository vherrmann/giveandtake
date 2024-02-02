export type Nullable<T> = T | null | undefined;

// Define a function to fmap over a nullable value
export function mapNullable<T, U>(
  value: Nullable<T>,
  mapper: (val: T) => U,
): Nullable<U> {
  switch (value) {
    case null:
      return null;
    case undefined:
      return undefined;
    default:
      return mapper(value);
  }
}

export function randomInRange(a: number, b: number) {
  return a + Math.floor(Math.random() * (b - a));
}

export function randomBool() {
  return Math.random() >= 0.5;
}

export function noteToVexflow(note: string) {
  const re = /^([A-G])(bb|b|#|##|)(\d)$/;
  const result = note.match(re);
  if (result) {
    const [_, name, acc, oct] = result;
    return `${name}${acc}/${oct}`;
  } else {
    throw new Error("Bad formatted note");
  }
}

export function chordToVexflow(notes: string[]) {
  return notes.map(noteToVexflow);
}
