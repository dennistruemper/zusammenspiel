import { Page } from "@playwright/test";

export abstract class BasePage {
  constructor(protected page: Page) {}

  async waitForPageLoad() {
    await this.page.waitForLoadState("domcontentloaded");
  }

  async waitForElement(selector: string) {
    await this.page.waitForSelector(selector);
  }
}
