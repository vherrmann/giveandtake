import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import checker from "vite-plugin-checker";
import eslint from "vite-plugin-eslint";

export default defineConfig({
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
      }),
      apply: "build",
    },
    {
      ...eslint(),
      apply: "serve",
    },
  ],
  // test: {
  //   globals: true,
  //   environment: "jsdom",
  //   setupFiles: ["./src/setupTests.js"],
  // },
});
