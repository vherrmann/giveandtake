import { format } from "date-fns";
import { useEffect, useState } from "react";
import { Api, User, UserPublic } from "./api";
import { AxiosPromise } from "axios";

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

export const handleApiErr = (error: any): string => {
  console.log(error);

  var errMsg;

  if (error.response) {
    // The request was made and the server responded with a status code
    // that falls out of the range of 2xx
    console.log(error.response.data);
    console.log(error.response.status);
    console.log(error.response.headers);
    errMsg = "Error:" + error.response.data;
  } else if (error.request) {
    // The request was made but no response was received
    // `error.request` is an instance of XMLHttpRequest in the browser and an instance of
    // http.ClientRequest in node.js
    console.log(error.request);
    errMsg = "Error: No response received: " + error.request;
  } else if (error.message) {
    // Something happened in setting up the request that triggered an Error
    console.log(error.message);
    errMsg = "Error: " + error.message;
  } else {
    console.log("Error: Unkown Error");
    return "Error: Unkown Error";
  }

  console.log(error.config);
  return errMsg;
};

export const withApi = async (
  fn: (api: ReturnType<typeof Api>) => Promise<void>,
  setError: React.Dispatch<React.SetStateAction<string | null>>,
  errorHandler?: (e: any) => void,
) => {
  try {
    await fn(Api());
  } catch (e) {
    try {
      if (errorHandler) {
        errorHandler(e);
      } else {
        setError(handleApiErr(e));
      }
    } catch (e: any) {
      setError(handleApiErr(e));
    }
  }
};

type AxiosPromiseType<T extends AxiosPromise<any>> =
  T extends AxiosPromise<infer U> ? U : never;

type ApiType = ReturnType<typeof Api>;
type ApiMethod = keyof ReturnType<typeof Api>;
type ApiReturnType<T> = AxiosPromiseType<ReturnType<ApiType[T & ApiMethod]>>;

type ApiStateFnRet<T> = [
  ApiReturnType<T> | null,
  React.Dispatch<React.SetStateAction<ApiReturnType<T> | null>>,
  boolean,
  string | null,
];

type EndpointToReqParam<T> =
  Parameters<ApiType[T & ApiMethod]> extends [
    requestParameters: infer R,
    options?: infer O,
  ]
    ? R
    : undefined;

type EndpointToOptions<T> =
  Parameters<ApiType[T & ApiMethod]> extends [options?: infer O]
    ? O
    : Parameters<ApiType[T & ApiMethod]> extends [
          requestParameters: infer R,
          options?: infer O,
        ]
      ? R
      : undefined;

// https://github.com/microsoft/TypeScript/issues/27808
// Here you define how your function overload will look
type ApiStateFn<OneOfPossibleOptions> = <const T extends OneOfPossibleOptions>(
  method: T,
  requestParameters: EndpointToReqParam<T>,
  options?: EndpointToOptions<T>,
  extraDeps?: any[],
  errorHandler?: (e: any) => void,
) => ApiStateFnRet<T>;

type IntersectionToRenderedIntersection<U> = (
  U extends any ? (_: ApiStateFn<U>) => void : never
) extends (_: infer I) => void
  ? I
  : never;

export const useApiState: IntersectionToRenderedIntersection<ApiMethod> = <
  T extends ApiMethod,
>(
  method: T,
  requestParameters: EndpointToReqParam<T>,
  options?: EndpointToOptions<T>,
  extraDeps?: any[],
  errorHandler?: (e: any) => void,
) => {
  type Data = AxiosPromiseType<ReturnType<ApiType[T]>>;
  const [data, setData] = useState<Data | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const fetchData = async () => {
    setLoading(true);
    setError(null);
    await withApi(
      async (api) => {
        if (requestParameters) {
          const response = await (api[method] as any)(
            requestParameters,
            options,
          );
          setData(response.data);
        } else {
          const response = await (api[method] as any)(options);
          setData(response.data);
        }
      },
      setError,
      errorHandler,
    );
    setLoading(false);
  };

  useEffect(() => {
    fetchData();
  }, [requestParameters, ...(extraDeps || [])]);

  const res: ApiStateFnRet<T> = [data, setData, loading, error];

  return res;
};

type Endpoints = {
  [Property in keyof ApiType]: Property;
};

// helper to have autocompletion for the strings of all endpoints, should be used like this:
/* const [friends, setFriends, loadingFr, errorFr] = useApiState(DApi.apiFriendsGet); */
const globalApi = Api();
// @ts-ignore
export const DApi: Endpoints = Object.fromEntries(
  Object.keys(globalApi).map((endpoint) => [endpoint, endpoint]),
);
