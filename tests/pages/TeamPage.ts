import { BasePage } from "./BasePage";

export class TeamPage extends BasePage {
  async openShareModal() {
    await this.page.getByRole("button", { name: "↗️Teilen" }).click();
  }

  async closeShareModal() {
    await this.page.getByRole("button", { name: "✕" }).click();
  }

  async getShareButton() {
    return this.page.getByRole("button", { name: "↗️Teilen" });
  }

  async getCloseButton() {
    return this.page.getByRole("button", { name: "✕" });
  }

  async getShareModal() {
    return this.page.getByRole("dialog");
  }

  async getTeamLinkInModal() {
    return this.page
      .getByRole("dialog")
      .locator("text=/team\\/share-test-team-/");
  }

  async getAccessCodeInModal() {
    return this.page.getByRole("dialog").locator("text=/^\\d{4}$/");
  }

  async copyTeamLink() {
    await this.page.getByRole("button", { name: "📋" }).first().click();
  }

  async copyAccessCode() {
    await this.page.getByRole("button", { name: "📋" }).nth(1).click();
  }

  async getCopyTeamLinkButton() {
    return this.page.getByRole("button", { name: "📋" }).first();
  }

  async getCopyAccessCodeButton() {
    return this.page.getByRole("button", { name: "📋" }).nth(1);
  }

  async getAccessCodeInput() {
    return this.page.getByPlaceholder("Zugangscode eingeben");
  }

  async submitAccessCode(accessCode: string) {
    const input = this.page.getByPlaceholder("Zugangscode eingeben");
    await input.fill(accessCode);
  }

  async getAccessCodeRequiredMessage() {
    return this.page.getByText("Zugangscode erforderlich");
  }

  async getTeamName() {
    return this.page.getByRole("heading", { level: 1 });
  }

  async getMatchesList() {
    return this.page.getByText("Kommende Spiele");
  }
}
