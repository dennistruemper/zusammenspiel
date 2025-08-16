# Playwright E2E Tests for Zusammenspiel

This directory contains end-to-end tests for the Zusammenspiel team management application using Playwright.

## 🚀 Quick Start

### Prerequisites

- Node.js (v16 or higher)
- Lamdera CLI installed and working

### Installation

```bash
npm install
npx playwright install
```

### Running Tests

#### Run all tests

```bash
npm test
```

#### Run tests in specific browser

```bash
npx playwright test --project=chromium
npx playwright test --project=firefox
npx playwright test --project=webkit
```

#### Run tests in mobile viewport

```bash
npx playwright test --project="Mobile Chrome"
npx playwright test --project="Mobile Safari"
```

#### Run tests in headed mode (see browser)

```bash
npx playwright test --headed
```

#### Run tests in debug mode

```bash
npx playwright test --debug
```

#### Run specific test file

```bash
npx playwright test team-creation.spec.ts
```

#### Run tests with specific pattern

```bash
npx playwright test -g "should create a team successfully"
```

## 📁 Test Structure

### `team-creation.spec.ts`

Core team creation flow tests covering:

- Team creation with form validation
- Access code generation and display
- Team sharing functionality
- Copy to clipboard operations
- URL-based team access
- Error handling

### `helpers/team-helpers.ts`

Reusable helper functions for:

- Team creation workflows
- Common assertions
- Clipboard operations
- Team member verification

## 🧪 Test Scenarios

### 1. Team Creation Flow

- ✅ Create team successfully
- ✅ Generate and display access code
- ✅ Show share modal with team link and access code
- ✅ Copy team link to clipboard
- ✅ Copy access code to clipboard
- ✅ Access team with URL and access code
- ✅ Handle invalid team access gracefully
- ✅ Validate form inputs before submission
- ✅ Handle team creation with minimum required fields

## 🔧 Configuration

### `playwright.config.ts`

- **Base URL**: `http://localhost:8000`
- **Web Server**: Automatically starts `lamdera dev`
- **Browsers**: Chrome, Firefox, Safari, Mobile Chrome, Mobile Safari
- **Screenshots**: On failure only
- **Traces**: On first retry
- **Retries**: 2 in CI, 0 locally

## 🚨 Troubleshooting

### Common Issues

#### Tests fail with "Page not found"

- Ensure Lamdera dev server is running
- Check if the app is accessible at `http://localhost:8000`
- Verify the app has the expected routes

#### Clipboard tests fail

- Some browsers require HTTPS for clipboard access
- Tests may need to run in headed mode for clipboard permissions
- Consider using `--headed` flag for clipboard tests

#### Mobile tests fail

- Ensure mobile viewport is properly configured
- Check if the app is responsive on mobile devices
- Verify touch interactions work correctly

### Debug Mode

Run tests with `--debug` flag to:

- Open browser in headed mode
- Pause execution at each step
- Inspect elements and state
- Step through test execution

## 📊 Test Reports

After running tests, view the HTML report:

```bash
npx playwright show-report
```

## 🔄 Continuous Integration

Tests are configured to run in CI environments:

- **Retries**: 2 attempts for flaky tests
- **Workers**: 1 worker to avoid conflicts
- **Web Server**: Automatic startup with timeout
- **Screenshots**: Captured on failures for debugging

## 🎯 Best Practices

1. **Use helper functions** for common operations
2. **Test user workflows** not implementation details
3. **Verify visual elements** are present and correct
4. **Test error conditions** and edge cases
5. **Use descriptive test names** that explain the scenario
6. **Keep tests independent** - each test should be self-contained
7. **Use page objects** for complex page interactions
8. **Test accessibility** with built-in Playwright features


