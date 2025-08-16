import { Page, expect } from "@playwright/test";
import { HomePage } from "../pages/HomePage";
import { TeamCreationPage } from "../pages/TeamCreationPage";
import { TeamPage } from "../pages/TeamPage";

export async function createTeam(
  page: Page,
  teamName: string,
  creatorName: string,
  members: string[]
): Promise<{ teamPage: TeamPage; accessCode: string }> {
  const homePage = new HomePage(page);
  const teamCreationPage = new TeamCreationPage(page);
  const teamPage = new TeamPage(page);

  // Navigate to home page
  await page.goto("/");

  // Wait for page to be ready
  await homePage.waitForPageLoad();

  // Navigate to create team page
  await homePage.navigateToCreateTeam();

  // Fill out the form
  await teamCreationPage.fillTeamName(teamName);
  await teamCreationPage.fillCreatorName(creatorName);
  await teamCreationPage.fillMembers(members);
  await teamCreationPage.fillPlayersNeeded("11"); // Default to 11 players

  // Submit the form
  await teamCreationPage.submitForm();

  // Wait for team page to load
  await expect(page).toHaveURL(
    new RegExp(`/team/${teamName.toLowerCase().replace(/\s+/g, "-")}-`)
  );

  // Get access code from URL
  const url = page.url();
  const accessCodeMatch = url.match(/[?&]code=(\d{4})/);
  const accessCode = accessCodeMatch ? accessCodeMatch[1] : "";

  return { teamPage, accessCode };
}

export async function openShareModal(page: Page): Promise<TeamPage> {
  const teamPage = new TeamPage(page);
  await teamPage.openShareModal();
  return teamPage;
}

export async function copyToClipboard(
  page: Page,
  buttonIndex: number = 0
): Promise<void> {
  const teamPage = new TeamPage(page);
  if (buttonIndex === 0) {
    await teamPage.copyTeamLink();
  } else {
    await teamPage.copyAccessCode();
  }
}

export async function verifyTeamMembers(
  page: Page,
  expectedMembers: string[]
): Promise<void> {
  // Verify all expected team members are visible
  for (const member of expectedMembers) {
    await expect(page.getByText(member)).toBeVisible();
  }
}

export async function verifyTeamPageLoaded(
  page: Page,
  teamName: string,
  creatorName: string
): Promise<void> {
  // Verify team page loads with correct information
  await expect(page.getByRole("heading", { name: teamName })).toBeVisible();
  await expect(page.getByText(creatorName)).toBeVisible();
}
