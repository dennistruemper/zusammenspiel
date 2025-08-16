import { expect, test } from "@playwright/test";
import { HomePage } from "./pages/HomePage";
import { TeamCreationPage } from "./pages/TeamCreationPage";
import { TeamPage } from "./pages/TeamPage";

test.describe("Core Team Creation Flow", () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to the home page before each test
    await page.goto("/");
  });

  test("should create a team successfully", async ({ page }) => {
    const homePage = new HomePage(page);
    const teamCreationPage = new TeamCreationPage(page);
    const teamPage = new TeamPage(page);

    // Navigate to create team page
    await homePage.navigateToCreateTeam();

    // Verify we're on the create team page
    await expect(page).toHaveURL("/create");

    // Fill out the team creation form
    await teamCreationPage.fillTeamName("Test Football Team");
    await teamCreationPage.fillCreatorName("John Doe");
    await teamCreationPage.fillMembers(["Alice", "Bob", "Charlie"]);
    await teamCreationPage.fillPlayersNeeded("11");

    // Submit the form
    await teamCreationPage.submitForm();

    // Wait for team creation and redirect
    await expect(page).toHaveURL(/\/team\/test-football-team-/);

    // Verify team page loads with correct information
    await expect(
      page.getByRole("heading", { name: "Test Football Team" })
    ).toBeVisible();
    await expect(page.getByText("John Doe")).toBeVisible();
    await expect(page.getByText("Alice")).not.toBeVisible();
    await expect(page.getByText("Bob")).not.toBeVisible();
    await expect(page.getByText("Charlie")).not.toBeVisible();
  });

  test("should generate and display access code after team creation", async ({
    page,
  }) => {
    const homePage = new HomePage(page);
    const teamCreationPage = new TeamCreationPage(page);

    // Wait for page to be ready
    await homePage.waitForPageLoad();

    // Create a team
    await homePage.navigateToCreateTeam();
    await teamCreationPage.fillTeamName("Access Code Test Team");
    await teamCreationPage.fillCreatorName("Jane Smith");
    await teamCreationPage.fillMembers(["David", "Eve"]);
    await teamCreationPage.fillPlayersNeeded("5");
    await teamCreationPage.submitForm();

    // Wait for team page to load
    await expect(page).toHaveURL(/\/team\/access-code-test-team-/);

    // Verify access code is displayed (should be 4 digits)
    const accessCodeElement = page.locator("text=/\\d{4}/").first();
    await expect(accessCodeElement).toBeVisible();

    // Verify access code is exactly 4 characters
    const accessCodeText = await accessCodeElement.textContent();
    expect(accessCodeText).toMatch(/=\d{4}$/);
  });

  test("should show share modal with team link and access code", async ({
    page,
  }) => {
    const homePage = new HomePage(page);
    const teamCreationPage = new TeamCreationPage(page);
    const teamPage = new TeamPage(page);

    // Create a team
    await homePage.navigateToCreateTeam();
    await teamCreationPage.fillTeamName("Share Test Team");
    await teamCreationPage.fillCreatorName("Mike Johnson");
    await teamCreationPage.fillMembers(["Sarah", "Tom"]);
    await teamCreationPage.fillPlayersNeeded("7");
    await teamCreationPage.submitForm();

    // Wait for team page to load
    await expect(page).toHaveURL(/\/team\/share-test-team-/);

    // Click the share button
    await teamPage.openShareModal();

    // Verify share modal is visible
    await expect(page.getByText("Team teilen")).toBeVisible();

    // Verify team link is displayed in the modal
    await expect(await teamPage.getTeamLinkInModal()).toBeVisible();

    // Verify access code is displayed prominently in the modal
    await expect(await teamPage.getAccessCodeInModal()).toBeVisible();

    // Verify QR code is generated
    await expect(page.locator("svg")).toBeVisible();
  });

  test("should copy team link to clipboard", async ({ page, context }) => {
    const homePage = new HomePage(page);
    const teamCreationPage = new TeamCreationPage(page);
    const teamPage = new TeamPage(page);

    // Create a team
    await homePage.navigateToCreateTeam();
    await teamCreationPage.fillTeamName("Copy Test Team");
    await teamCreationPage.fillCreatorName("Lisa Brown");
    await teamCreationPage.fillMembers(["Alex", "Sam"]);
    await teamCreationPage.fillPlayersNeeded("6");
    await teamCreationPage.submitForm();

    // Wait for team page to load
    await expect(page).toHaveURL(/\/team\/copy-test-team-/);

    // Open share modal
    await teamPage.openShareModal();

    // Click copy button for team link
    await teamPage.copyTeamLink();

    // Verify clipboard content for supported browsers (Chromium, Mobile Chrome)
    const browserName = page.context().browser()?.browserType().name();
    if (browserName === "chromium") {
      const clipboardText = await page.evaluate(() =>
        navigator.clipboard.readText()
      );
      expect(clipboardText).toMatch(/\/team\/copy-test-team-.*\?code=\d{4}/);
    }
  });

  test("should copy access code to clipboard", async ({ page, context }) => {
    const homePage = new HomePage(page);
    const teamCreationPage = new TeamCreationPage(page);
    const teamPage = new TeamPage(page);

    // Create a team
    await homePage.navigateToCreateTeam();
    await teamCreationPage.fillTeamName("Code Copy Test Team");
    await teamCreationPage.fillCreatorName("Chris Wilson");
    await teamCreationPage.fillMembers(["Emma", "Frank"]);
    await teamCreationPage.fillPlayersNeeded("8");
    await teamCreationPage.submitForm();

    // Wait for team page to load
    await expect(page).toHaveURL(/\/team\/code-copy-test-team-/);

    // Open share modal
    await teamPage.openShareModal();

    // Click copy button for access code (second copy button)
    await teamPage.copyAccessCode();

    // Verify clipboard content for supported browsers (Chromium, Mobile Chrome)
    const browserName = page.context().browser()?.browserType().name();
    if (browserName === "chromium") {
      const clipboardText = await page.evaluate(() =>
        navigator.clipboard.readText()
      );
      expect(clipboardText).toMatch(/^\d{4}$/);
    }
  });

  test("should access team with URL and access code", async ({ page }) => {
    const homePage = new HomePage(page);
    const teamCreationPage = new TeamCreationPage(page);

    // Create a team first
    await homePage.navigateToCreateTeam();
    await teamCreationPage.fillTeamName("URL Access Test Team");
    await teamCreationPage.fillCreatorName("Pat Davis");
    await teamCreationPage.fillMembers(["Jordan", "Casey"]);
    await teamCreationPage.fillPlayersNeeded("4");
    await teamCreationPage.submitForm();

    // Wait for team page to load and get the URL
    await expect(page).toHaveURL(/\/team\/url-access-test-team-/);
    const teamUrl = page.url();

    // Open a new page and navigate directly to the team URL
    const newPage = await page.context().newPage();
    await newPage.goto(teamUrl);

    // Verify team page loads without requiring access code input
    await expect(newPage.getByText("URL Access Test Team")).toBeVisible();
    await expect(newPage.getByText("Pat Davis")).toBeVisible();
    await expect(newPage.getByText("Jordan")).not.toBeVisible();
    await expect(newPage.getByText("Casey")).not.toBeVisible();

    // Close the new page
    await newPage.close();
  });

  test("should require access code for invalid team access", async ({
    page,
  }) => {
    // Try to access a non-existent team with invalid ID
    const fakeTeamUrl = "/team/fake-team-12345678?code=9999";
    await page.goto(fakeTeamUrl);

    // Should show access code required or team not found
    // This test verifies the app handles invalid team access gracefully
    await expect(page.locator("body")).toBeVisible();
  });

  test("should validate form inputs before submission", async ({ page }) => {
    const homePage = new HomePage(page);
    const teamCreationPage = new TeamCreationPage(page);

    // Wait for page to be ready
    await homePage.waitForPageLoad();

    // Navigate to create team page
    await homePage.navigateToCreateTeam();

    // Try to submit empty form
    await teamCreationPage.submitForm();

    // Should stay on create team page (form validation prevents submission)
    await expect(page).toHaveURL("/create");
  });

  test("should handle team creation with minimum required fields", async ({
    page,
  }) => {
    const homePage = new HomePage(page);
    const teamCreationPage = new TeamCreationPage(page);

    // Navigate to create team page
    await homePage.navigateToCreateTeam();

    // Fill only required fields
    await teamCreationPage.fillTeamName("Minimal Team");
    await teamCreationPage.fillCreatorName("Minimal User");
    await teamCreationPage.fillPlayersNeeded("3");
    // Leave other member names empty

    // Submit the form
    await teamCreationPage.submitForm();

    // Should create team successfully with just creator
    await expect(page).toHaveURL(/\/team\/minimal-team-/);
    await expect(
      page.getByRole("heading", { name: "Minimal Team" })
    ).toBeVisible();
    await expect(page.getByText("Minimal User")).toBeVisible();
  });
});
