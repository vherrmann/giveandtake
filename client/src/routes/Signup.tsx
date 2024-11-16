import React, { useState } from "react";
import { Button, Stack, TextField, Tooltip, Typography } from "@mui/material";
import { Api } from "../api";
import { useSearchParams } from "react-router-dom";
import { handleApiErr, useTitle } from "../utils";
import { userNameMaxLength } from "../consts";
import { LinkWidget } from "../widgets/LinkWidget";

interface SignupData {
  name: string;
  email: string;
  password: string;
}

export const Signup = (): JSX.Element => {
  useTitle(`Signup`);
  const [signupData, setSignupData] = useState<SignupData>({
    name: "",
    email: "",
    password: "",
  });
  const [confirmPassword, setConfirmPassword] = useState<string>("");

  const [error, setError] = useState<string>("");
  const [info, setInfo] = useState<string>("");
  const [loading, setLoading] = useState<boolean>(false);
  const [succeeded, setSucceeded] = useState<boolean>(false);

  const [searchParams, setSearchParams] = useSearchParams();
  const secret = searchParams.get("secret");

  const handleChangeSD = (e: React.ChangeEvent<HTMLInputElement>) => {
    // limit size of username to 20 characters
    if (e.target.name === "name" && e.target.value.length > userNameMaxLength) {
      return;
    }
    setSignupData({
      ...signupData,
      [e.target.name]: e.target.value,
    });
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (
      !secret ||
      !signupData.name ||
      !signupData.email ||
      !signupData.password ||
      !confirmPassword
    ) {
      setError("Please fill out all fields.");
      return;
    }

    if (signupData.password !== confirmPassword) {
      setError("Passwords do not match.");
      return;
    }

    // FIXME: demand strong passwords: The password needs to start with …
    // https://react-hook-form.com/
    // https://www.npmjs.com/package/yup
    try {
      setInfo("Sending signup request.... Sending verification email....");
      setLoading(true);
      const _jobId = await Api.apiAuthSignupPost({
        signupData: { ...signupData, secret },
      });
      setInfo("Sending verification email!");
      setError("");
      setSucceeded(true);
    } catch (e: any) {
      setError(handleApiErr(e));
    } finally {
      setLoading(false);
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <Stack spacing={2} alignItems="center">
        <h2>Sign Up</h2>
        <Tooltip title="Provided by the admin" placement="right">
          <TextField
            type="text"
            name="secret"
            variant="standard"
            value={secret}
            onChange={(e) => setSearchParams({ secret: e.target.value })}
            placeholder="Secret code"
            required
          />
        </Tooltip>
        <TextField
          type="text"
          name="name"
          variant="standard"
          value={signupData.name}
          onChange={handleChangeSD}
          placeholder="Name"
          required
        />
        <TextField
          type="email"
          name="email"
          variant="standard"
          value={signupData.email}
          onChange={handleChangeSD}
          placeholder="Email"
          required
        />
        <TextField
          type="password"
          name="password"
          variant="standard"
          value={signupData.password}
          onChange={handleChangeSD}
          placeholder="Password"
          required
        />
        <TextField
          type="password"
          name="confirmPassword"
          variant="standard"
          value={confirmPassword}
          onChange={(e) => setConfirmPassword(e.target.value)}
          placeholder="Confirm password"
          required
        />
        <Button type="submit" variant="contained" disabled={loading}>
          Sign up
        </Button>
        {succeeded && (
          <Typography>
            You should receive an email with a verification link. Please click
            on it to complete the signup process. Click{" "}
            <LinkWidget
              underline={"always"}
              to={`/requestEmailVerif?email=${encodeURIComponent(signupData.email)}`}
              color="primary"
            >
              here
            </LinkWidget>{" "}
            to request a new email click.
          </Typography>
        )}
        {error && <p style={{ color: "red" }}>Error: {error}</p>}
        {info && <p style={{ color: "green" }}>{info}</p>}
      </Stack>
    </form>
  );
};
