# Playwright E2E Testing for Zusammenspiel

This directory contains end-to-end tests for the Zusammenspiel team management application using Playwright.

## Quick Start

1. **Install dependencies** (if not already done):

   ```bash
   npm install
   ```

2. **Install Playwright browsers**:

   ```bash
   npx playwright install
   ```

3. **Run tests**:
   ```bash
   npm run test
   ```

## Installation

The project uses Playwright for E2E testing. Dependencies are managed through npm:

- **Playwright**: Core testing framework
- **TypeScript**: For type-safe test development
- **Playwright Test**: Test runner and assertions

## Running Tests

### Basic Test Execution

```bash
npm run test
```

### Specific Test Files

```bash
npx playwright test tests/team-creation.spec.ts
```

### Debug Mode

```bash
npx playwright test --debug
```

### Playwright Studio (Interactive GUI)

```bash
npx playwright studio
```

## Test Structure

### Page Object Model (POM) Pattern

**CRITICAL: All new tests MUST use the Page Object Model pattern for maintainability and consistency.**

The Page Object Model is implemented with the following structure:

```
tests/
├── pages/           # Page Object classes
│   ├── BasePage.ts  # Abstract base class with common functionality
│   ├── HomePage.ts  # Home page interactions
│   ├── TeamCreationPage.ts  # Team creation form interactions
│   └── TeamPage.ts  # Team page interactions
├── helpers/         # Reusable test helper functions
└── *.spec.ts        # Test specifications
```

#### Page Object Guidelines

1. **Extend BasePage**: All page objects must extend `BasePage` for common functionality
2. **Encapsulate Selectors**: All element locators must be defined within page objects
3. **Provide Action Methods**: Each page object should provide methods for common user actions
4. **Return Locators**: Methods that need to be used in assertions should return `Locator` objects
5. **Use Descriptive Names**: Method names should clearly describe the action being performed

#### Example Usage

```typescript
// ✅ CORRECT: Using POM pattern
const homePage = new HomePage(page);
const teamCreationPage = new TeamCreationPage(page);

await homePage.navigateToCreateTeam();
await teamCreationPage.fillTeamName("Test Team");
await expect(await teamCreationPage.getTeamNameInput()).toBeVisible();

// ❌ INCORRECT: Direct page interactions
await page.getByRole("link", { name: "Dein Team erstellen" }).click();
await page.getByPlaceholder("Team Name").fill("Test Team");
```

#### Adding New Page Objects

When adding new pages or components:

1. Create a new class extending `BasePage`
2. Define all element locators as private methods
3. Provide public methods for user actions
4. Update this README with the new page object
5. Ensure all tests use the new page object

### Test Organization

Tests are organized by feature/functionality:

- **`team-creation.spec.ts`**: Core team creation flow
- Future test files should follow the naming convention: `{feature}.spec.ts`

## Test Scenarios

### Core Team Creation Flow

Tests the complete team creation process:

1. **Navigation**: Home → Create Team page
2. **Form Filling**: Team name, creator name, members
3. **Submission**: Form submission and validation
4. **Redirection**: Success redirect to team page
5. **Verification**: Team page loads with correct data

### Share Functionality

Tests team sharing features:

1. **Modal Opening**: Share button click opens modal
2. **Content Display**: Team link and access code visible
3. **Copy Functionality**: Clipboard operations for link and code
4. **Modal Closing**: Proper modal dismissal

### Access Code Protection

Tests the access code system:

1. **Code Generation**: Automatic access code creation
2. **Code Display**: Access code shown in share modal
3. **Code Validation**: Backend validation of access codes

## Configuration

### Playwright Config (`playwright.config.ts`)

- **Base URL**: `http://localhost:8000` (Lamdera dev server)
- **Web Server**: Automatically starts Lamdera server for tests
- **Browsers**: Chromium, Firefox, WebKit, Mobile Chrome, Mobile Safari
- **Timeouts**: Optimized for development (reduced from defaults)
- **Parallelism**: 8 workers for local development, 2 for CI
- **Permissions**: Clipboard access enabled for Chromium browsers

### Test Timeouts

- **Test timeout**: 10 seconds
- **Expect timeout**: 2 seconds
- **Action timeout**: 2 seconds
- **Navigation timeout**: 5 seconds

## Troubleshooting

### Common Issues

1. **Element Not Found**: Check if selectors match current UI
2. **Timeout Errors**: Verify page load state and element visibility
3. **Clipboard Errors**: Only Chromium browsers support clipboard in tests
4. **Server Connection**: Ensure Lamdera server is running on port 8000

### Debug Commands

```bash
# Run specific test with debug output
npx playwright test --debug tests/team-creation.spec.ts

# Open Playwright Studio for visual debugging
npx playwright studio

# Run with headed browser
npx playwright test --headed
```

## Best Practices

### Test Design

1. **Use POM Pattern**: Always use page objects for element interactions
2. **Descriptive Names**: Test names should clearly describe the scenario
3. **Single Responsibility**: Each test should verify one specific behavior
4. **Setup/Teardown**: Use `beforeEach`/`afterEach` for common setup

### Element Selection

1. **Prefer Semantic Selectors**: Use `getByRole`, `getByText` over CSS selectors
2. **Use Placeholders**: For form inputs, prefer `getByPlaceholder` over `getByLabel`
3. **Avoid Index Selectors**: Use specific text or role when possible
4. **Wait for Elements**: Always wait for elements to be ready before interaction

### Assertions

1. **Verify State Changes**: Check both positive and negative conditions
2. **Use Appropriate Matchers**: Choose the right assertion for the data type
3. **Handle Async Operations**: Properly await asynchronous operations
4. **Browser-Specific Logic**: Handle browser differences (e.g., clipboard permissions)

### Maintenance

1. **Update Selectors**: Keep selectors in sync with UI changes
2. **Refactor Common Code**: Extract reusable patterns to page objects
3. **Document Changes**: Update this README when adding new patterns
4. **Regular Review**: Periodically review and update test patterns

## Future Enhancements

### Planned Test Coverage

- [ ] Match management (create, edit, delete)
- [ ] Availability tracking
- [ ] User authentication
- [ ] Team member management
- [ ] Mobile responsiveness
- [ ] Accessibility compliance

### Performance Testing

- [ ] Load testing for multiple concurrent users
- [ ] Performance benchmarks for key operations
- [ ] Memory leak detection
- [ ] Network latency simulation

### Visual Testing

- [ ] Screenshot comparison tests
- [ ] Visual regression testing
- [ ] Cross-browser visual consistency
- [ ] Responsive design validation
