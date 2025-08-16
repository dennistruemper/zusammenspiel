import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./tests",
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: 2,
  workers: process.env.CI ? 2 : 8, // Increased parallelism: 2 for CI, 8 for local development
  reporter: "list",
  timeout: 10000, // 10 seconds global timeout
  expect: {
    timeout: 5000, // 5 seconds for expect assertions
  },
  use: {
    baseURL: "http://localhost:8889",
    trace: "on-first-retry",
    screenshot: "only-on-failure",
    actionTimeout: 2000, // 2 seconds for actions like click, fill
    navigationTimeout: 2000, // 2 seconds for navigation
  },
  projects: [
    {
      name: "chromium",
      use: {
        ...devices["Desktop Chrome"],
        contextOptions: {
          permissions: ["clipboard-read", "clipboard-write"],
        },
      },
    },
    {
      name: "firefox",
      use: { ...devices["Desktop Firefox"] },
    },
    {
      name: "webkit",
      use: { ...devices["Desktop Safari"] },
    },
    {
      name: "Mobile Chrome",
      use: {
        ...devices["Pixel 5"],
        contextOptions: {
          permissions: ["clipboard-read", "clipboard-write"],
        },
      },
    },
    {
      name: "Mobile Safari",
      use: { ...devices["iPhone 12"] },
    },
  ],
  webServer: {
    command: "lamdera live --port 8889",
    url: "http://localhost:8889",
    reuseExistingServer: !process.env.CI,
    timeout: 10000, // 10 seconds for server startup
  },
});
