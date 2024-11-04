import { createContext, useContext, ReactNode, useEffect } from "react";
import { useLocalStorage } from "../utils";
import { Api, Configuration, LoginData, User } from "../api";

export interface AuthProps {
  isAuthenticated: true;
  userId: string;
  user: User;
}

export interface UnauthProps {
  isAuthenticated: false;
  userId: null;
  user: null;
}
export type AuthContextType = {
  login: (loginData: LoginData) => Promise<[boolean, string]>;
  logout: () => Promise<void>;
  authState: AuthProps | UnauthProps;
};

const AuthContext = createContext<AuthContextType | undefined>(undefined);

export const AuthProvider = ({ children }: { children: ReactNode }) => {
  const api = Api();
  // FIXME: check if cookie is still there and fetch information
  const [authState, setAuthState] = useLocalStorage<AuthProps | UnauthProps>(
    "authState",
    { isAuthenticated: false, userId: null, user: null },
  );

  const fetchAuthState = async () => {
    try {
      const response = await api.apiAuthCheckGet();
      setAuthState({
        isAuthenticated: true,
        userId: response.data.userId,
        user: response.data.user,
      });
    } catch (error: any) {
      if (authState.isAuthenticated) {
        console.log("User is not authenticated anymore. Logging out.");
        setAuthState({ isAuthenticated: false, userId: null, user: null });
      }
    }
  };

  useEffect(() => {
    fetchAuthState();
  }, []);

  const login = async ({
    email,
    password,
  }: LoginData): Promise<[boolean, string]> => {
    try {
      const response = await api.apiAuthLoginPost({
        email,
        password,
      });
      setAuthState({
        isAuthenticated: true,
        userId: response.data.userId,
        user: response.data.user,
      });
      return [true, ""];
    } catch (error: any) {
      if (error.response) {
        const msg = error.response.data;
        return [false, msg];
      } else {
        return [false, error.message];
      }
    }
  };
  const logout = async () => {
    try {
      await api.apiAuthLogoutPost();
      setAuthState({ isAuthenticated: false, userId: null, user: null });
    } catch (error: any) {
      // FIXME: more information
      // FIXME: inform user
      console.error(error);
    }
  };

  return (
    <AuthContext.Provider value={{ authState, login, logout }}>
      {children}
    </AuthContext.Provider>
  );
};

export const useAuth = () => {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error("useAuth must be used within an AuthProvider");
  }
  return context;
};

export const useAuthState = () => {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error("useAuthState must be used within an AuthProvider");
  }
  return context.authState;
};
