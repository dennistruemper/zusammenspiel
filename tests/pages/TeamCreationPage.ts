import { BasePage } from "./BasePage";

export class TeamCreationPage extends BasePage {
  async fillTeamName(teamName: string) {
    await this.page.getByPlaceholder("Teamname eingeben").fill(teamName);
  }

  async fillCreatorName(creatorName: string) {
    await this.page.getByPlaceholder("Dein Name").fill(creatorName);
  }

  async fillMembers(members: string[]) {
    const membersText = members.join(", ");
    await this.page
      .getByPlaceholder("Namen durch Komma getrennt")
      .fill(membersText);
  }

  async fillPlayersNeeded(playersNeeded: string) {
    await this.page.getByPlaceholder("z.B. 11 für Fußball").fill(playersNeeded);
  }

  async submitForm() {
    await this.page.getByRole("button", { name: "Team erstellen" }).click();
  }

  async getTeamNameInput() {
    return this.page.getByPlaceholder("Teamname eingeben");
  }

  async getCreatorNameInput() {
    return this.page.getByPlaceholder("Dein Name");
  }

  async getMemberInput(index: number) {
    return this.page.getByPlaceholder("Namen durch Komma getrennt");
  }

  async getSubmitButton() {
    return this.page.getByRole("button", { name: "Team erstellen" });
  }
}
