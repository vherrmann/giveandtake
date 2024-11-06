import { useEffect, useState } from "react";
import { useSearchParams } from "react-router-dom";
import { Api } from "../api";
import { handleApiErr } from "../utils";

export const VerifyEmail = () => {
  const [searchParams] = useSearchParams();
  const secret = searchParams.get("secret");
  const user = searchParams.get("userId");
  const api = Api();

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
    } catch (e: any) {
      setError(handleApiErr(e));
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
