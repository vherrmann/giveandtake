import { useEffect, useState } from "react";
import { useSearchParams } from "react-router-dom";
import { Api } from "../api";

export const VerifyEmail = () => {
  const [searchParams] = useSearchParams();
  const secret = searchParams.get("secret");
  const user = searchParams.get("userId");
  const api = new Api();

  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const requestVerification = async () => {
    try {
      if (!secret || !user) {
        throw new Error("Invalid verification link");
      }
      await api.apiAuthVerifyemailPost({ verifyEmail: { user, secret } });
      setLoading(false);
      setError(null);
    } catch (error: any) {
      if (error.response) {
        setError(await error.response.text());
      } else {
        setError(error.message);
      }
      setLoading(false);
    }
  };

  useEffect(() => {
    requestVerification();
  }, []);

  if (loading) {
    return <p>Sending verification request...</p>;
  }

  if (error !== null) {
    return <p>Error: {error}</p>;
  }

  return <p>Email verified successfully</p>;
};
