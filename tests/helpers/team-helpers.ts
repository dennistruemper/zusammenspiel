import { Page, expect } from "@playwright/test";

export interface TeamCreationData {
  name: string;
  creatorName: string;
  otherMemberNames: string;
  playersNeeded: string;
}

export async function createTeam(
  page: Page,
  teamData: TeamCreationData
): Promise<string> {
  // Navigate to create team page
  await page.getByRole("link", { name: "Mannschaft erstellen" }).click();

  // Verify we're on the create team page
  await expect(page).toHaveURL("/create");
  await expect(
    page.getByRole("heading", { name: "Neue Mannschaft erstellen" })
  ).toBeVisible();

  // Fill out the team creation form
  await page.getByLabel("Mannschaftsname").fill(teamData.name);
  await page.getByLabel("Dein Name").fill(teamData.creatorName);
  await page
    .getByLabel("Weitere Teammitglieder (durch Komma getrennt)")
    .fill(teamData.otherMemberNames);
  await page
    .getByLabel("Wie viele Spieler werden für ein Spiel benötigt?")
    .fill(teamData.playersNeeded);

  // Submit the form
  await page.getByRole("button", { name: "Mannschaft erstellen" }).click();

  // Wait for team creation and redirect
  const expectedUrlPattern = new RegExp(
    `/team/${teamData.name.toLowerCase().replace(/\s+/g, "-")}-`
  );
  await expect(page).toHaveURL(expectedUrlPattern);

  // Return the team URL for further testing
  return page.url();
}

export async function openShareModal(page: Page): Promise<void> {
  // Click the share button
  await page.getByRole("button", { name: "↗️ Teilen" }).click();

  // Verify share modal is visible
  await expect(page.getByRole("dialog")).toBeVisible();
  await expect(page.getByText("Team teilen")).toBeVisible();
}

export async function getAccessCodeFromPage(page: Page): Promise<string> {
  // Look for the access code (4 digits) on the page
  const accessCodeElement = page.locator("text=/\\d{4}/").first();
  await expect(accessCodeElement).toBeVisible();

  const accessCodeText = await accessCodeElement.textContent();
  if (!accessCodeText || !accessCodeText.match(/^\d{4}$/)) {
    throw new Error("Access code not found or invalid format");
  }

  return accessCodeText;
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

export async function copyToClipboard(
  page: Page,
  copyButtonIndex: number = 0
): Promise<string> {
  // Click the copy button (first one by default)
  await page.getByRole("button", { name: "📋" }).nth(copyButtonIndex).click();

  // Get the clipboard content
  const clipboardText = await page
    .context()
    .evaluate(() => navigator.clipboard.readText());
  return clipboardText;
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

export const sampleTeamData: TeamCreationData = {
  name: "Test Team",
  creatorName: "Test Creator",
  otherMemberNames: "Member 1, Member 2, Member 3",
  playersNeeded: "11",
};


