import { ReactNode } from "react";
import { Navigate, useLocation } from "react-router-dom";
import { AuthProps, useAuthState } from "./providers/auth";

function RequireAuth({ children }: { children: ReactNode }) {
  const { isAuthenticated } = useAuthState();
  const location = useLocation();

  return isAuthenticated ? (
    <>{children}</>
  ) : (
    <Navigate to="/login" replace state={{ path: location.pathname }} />
  );
}

export const ProtectedRoute = ({ element, ...rest }: any) => {
  return { ...rest, element: <RequireAuth> {element} </RequireAuth> };
};

export const useAuthedState = (): AuthProps => {
  const state = useAuthState();

  if (!state.isAuthenticated) {
    throw new Error("useAuthedState must be used within an RequireAuth");
  }

  return state;
};
