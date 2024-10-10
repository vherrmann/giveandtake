import { format } from "date-fns";
import { useEffect, useState } from "react";
import { User, UserPublic } from "./api";

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

export function formatDate(date: string) {
  return format(date, "MMMM d, yyyy");
}

export function getCurrDate(): string {
  return Date();
}

export function useLocalStorage<T>(
  key: string,
  initialValue: T,
): [T, React.Dispatch<React.SetStateAction<T>>] {
  // Initialize state with value from localStorage or fallback to initialValue
  const [storedValue, setStoredValue] = useState<T>(() => {
    try {
      const item = window.localStorage.getItem(key);
      // Parse stored JSON string or return initial value
      return item ? JSON.parse(item) : initialValue;
    } catch (error) {
      console.error("Failed to retrieve data from localStorage", error);
      return initialValue;
    }
  });

  // Update localStorage whenever state changes
  useEffect(() => {
    try {
      window.localStorage.setItem(key, JSON.stringify(storedValue));
    } catch (error) {
      console.error("Failed to save data to localStorage", error);
    }
  }, [key, storedValue]);

  return [storedValue, setStoredValue];
}

export const userToUserPublic = (user: User): UserPublic => {
  return {
    name: user.name,
    createdAt: user.createdAt,
  };
};

// https://stackoverflow.com/a/66494926/13534562
export const stringToColor = (str: string): string => {
  let stringUniqueHash = [...str].reduce((acc, char) => {
    return char.charCodeAt(0) + ((acc << 5) - acc);
  }, 0);
  return `hsl(${stringUniqueHash % 360}, 95%, 35%)`;
};

export const redirect = (to: string) => {
  window.location.href = to;
};

export const HardRedirect = ({ to }: { to: string }) => {
  useEffect(() => {
    window.location.href = to;
  }, []);

  return null;
};

export const showApiErr = async (
  error: any,
  setError: React.Dispatch<React.SetStateAction<any>>,
) => {
  if (error.response) {
    setError(await error.response.text());
  } else {
    setError(error.message);
  }
};
