import { BasePage } from "./BasePage";

export class HomePage extends BasePage {
  async navigateToCreateTeam() {
    await this.page.getByRole("link", { name: "Dein Team erstellen" }).click();
  }

  async getCreateTeamLink() {
    return this.page.getByRole("link", { name: "Dein Team erstellen" });
  }
}
