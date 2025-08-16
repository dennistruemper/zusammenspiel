const { chromium } = require("@playwright/test");

async function checkCreatePage() {
  const browser = await chromium.launch({ headless: false });
  const page = await browser.newPage();

  try {
    console.log("🌐 Navigating to http://localhost:8889...");
    await page.goto("http://localhost:8889");

    // Wait for the page to load
    await page.waitForLoadState("networkidle");
    console.log("✅ Home page loaded");

    // Wait a bit more for Elm to render
    await page.waitForTimeout(2000);

    // Click on the "Dein Team erstellen" link
    console.log('🔗 Clicking on "Dein Team erstellen" link...');
    await page.getByRole("link", { name: "Dein Team erstellen" }).click();

    // Wait for navigation
    await page.waitForURL("**/create");
    console.log("✅ Navigated to create team page");

    // Wait for the page to load
    await page.waitForLoadState("networkidle");
    await page.waitForTimeout(2000);

    // Check for the heading
    const heading = page.getByRole("heading", {
      name: "Neue Mannschaft erstellen",
    });
    if (await heading.isVisible()) {
      console.log('✅ Found heading "Neue Mannschaft erstellen"');
    } else {
      console.log('❌ Heading "Neue Mannschaft erstellen" not found');
    }

    // Check for the submit button
    const submitButton = page.getByRole("button", {
      name: "Mannschaft erstellen",
    });
    if (await submitButton.isVisible()) {
      console.log('✅ Found submit button "Mannschaft erstellen"');
    } else {
      console.log('❌ Submit button "Mannschaft erstellen" not found');
    }

    // List all buttons on the page
    const allButtons = await page.locator("button").all();
    console.log(`\n📋 Found ${allButtons.length} buttons on the create page:`);

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
    await page.screenshot({ path: "create-page-debug.png", fullPage: true });
    console.log("📸 Screenshot saved as create-page-debug.png");
  } catch (error) {
    console.error("❌ Error:", error.message);
    console.error("Stack:", error.stack);
  } finally {
    await browser.close();
  }
}

checkCreatePage();
