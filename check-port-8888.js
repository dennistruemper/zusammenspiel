const { chromium } = require("@playwright/test");

async function checkPort8888() {
  const browser = await chromium.launch({ headless: false });
  const page = await browser.newPage();

  try {
    console.log("🌐 Navigating to http://localhost:8888...");
    await page.goto("http://localhost:8888");

    // Wait for the page to load
    await page.waitForLoadState("networkidle");
    console.log("✅ Page loaded");

    // Wait a bit more for Elm to render
    await page.waitForTimeout(2000);

    // Get the page title
    const title = await page.title();
    console.log(`📄 Page title: "${title}"`);

    // List all buttons on the page
    const allButtons = await page.locator("button").all();
    console.log(`\n📋 Found ${allButtons.length} buttons:`);

    for (let i = 0; i < allButtons.length; i++) {
      const button = allButtons[i];
      const text = await button.textContent();
      const ariaLabel = await button.getAttribute("aria-label");
      const isVisible = await button.isVisible();
      console.log(
        `  ${i + 1}. Text: "${text?.trim() || "N/A"}" | Aria-label: "${
          ariaLabel || "N/A"
        }" | Visible: ${isVisible}`
      );
    }

    // List all links on the page
    const allLinks = await page.locator("a").all();
    console.log(`\n🔗 Found ${allLinks.length} links:`);

    for (let i = 0; i < allLinks.length; i++) {
      const link = allLinks[i];
      const text = await link.textContent();
      const href = await link.getAttribute("href");
      const isVisible = await link.isVisible();
      console.log(
        `  ${i + 1}. Text: "${text?.trim() || "N/A"}" | Href: "${
          href || "N/A"
        }" | Visible: ${isVisible}`
      );
    }

    // List all form labels
    const allLabels = await page.locator("label").all();
    console.log(`\n🏷️ Found ${allLabels.length} form labels:`);

    for (let i = 0; i < allLabels.length; i++) {
      const label = allLabels[i];
      const text = await label.textContent();
      const isVisible = await label.isVisible();
      console.log(
        `  ${i + 1}. Text: "${text?.trim() || "N/A"}" | Visible: ${isVisible}`
      );
    }

    // Take a screenshot for debugging
    await page.screenshot({ path: "port-8888-debug.png", fullPage: true });
    console.log("📸 Screenshot saved as port-8888-debug.png");
  } catch (error) {
    console.error("❌ Error:", error.message);
    console.error("Stack:", error.stack);
  } finally {
    await browser.close();
  }
}

checkPort8888();
