import { defineConfig, UserConfig } from "vite";
import react from "@vitejs/plugin-react";
import checker from "vite-plugin-checker";
import eslint from "vite-plugin-eslint";
import { Options as EsLintOptions } from "vite-plugin-eslint";

const eslintBaseConfig: EsLintOptions["baseConfig"] = {
  ignorePatterns: ["src/api/autogen/"],
  rules: {
    "@typescript-eslint/no-unused-vars": [
      "warn",
      {
        args: "all",
        argsIgnorePattern: "^_",
        caughtErrors: "all",
        caughtErrorsIgnorePattern: "^_",
        destructuredArrayIgnorePattern: "^_",
        varsIgnorePattern: "^_",
        ignoreRestSiblings: true,
      },
    ],
  },
  extends: ["react-app", "react-app/jest"],
};

export default {
  publicDir: "public",
  plugins: [
    react(),
    checker({
      typescript: true,
    }),
    {
      // fail on warning in build
      ...eslint({
        failOnWarning: true,
        baseConfig: eslintBaseConfig,
      }),
      apply: "build",
    },
    {
      ...eslint({ baseConfig: eslintBaseConfig }),
      apply: "serve",
    },
  ],
  // test: {
  //   globals: true,
  //   environment: "jsdom",
  //   setupFiles: ["./src/setupTests.js"],
  // },
} satisfies UserConfig;
