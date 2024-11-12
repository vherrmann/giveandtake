import { useCallback, useEffect, useState } from "react";
import { useSearchParams } from "react-router-dom";
import { Api } from "../api";
import { handleApiErr } from "../utils";

export const VerifyEmail = () => {
  const [searchParams] = useSearchParams();
  const secret = searchParams.get("secret");
  const id = searchParams.get("id");

  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const requestVerification = useCallback(async () => {
    try {
      if (!secret || !id) {
        throw new Error("Invalid verification link");
      }
      await Api.apiAuthVerifyemailFinishPost({ verifyEmail: { id, secret } });
      setLoading(false);
      setError(null);
    } catch (e: any) {
      setError(handleApiErr(e));
      setLoading(false);
    }
  }, [id, secret]);

  useEffect(() => {
    requestVerification();
  }, [requestVerification]);

  if (loading) {
    return <p>Sending verification request...</p>;
  }

  if (error !== null) {
    return <p>Error: {error}</p>;
  }

  return <p>Email verified successfully</p>;
};
