const { chromium } = require("@playwright/test");

async function inspectPage() {
  const browser = await chromium.launch({ headless: false });
  const page = await browser.newPage();

  try {
    console.log("🌐 Navigating to http://localhost:8889...");
    await page.goto("http://localhost:8889");

    // Wait for the page to load
    await page.waitForLoadState("networkidle");
    console.log("✅ Page loaded");

    // Wait a bit more for Elm to render
    await page.waitForTimeout(2000);

    // Get the page title
    const title = await page.title();
    console.log(`📄 Page title: "${title}"`);

    // Check if Elm element exists
    const elmElement = await page.locator("#elm").isVisible();
    console.log(`🔍 Elm element visible: ${elmElement}`);

    // Get all text content
    const bodyText = await page.locator("body").textContent();
    console.log(
      `📝 Body text (first 500 chars): "${bodyText?.substring(0, 500)}..."`
    );

    // Check for any links
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

    // Check for any buttons
    const allButtons = await page.locator("button").all();
    console.log(`\n📋 Found ${allButtons.length} buttons:`);

    for (let i = 0; i < allButtons.length; i++) {
      const button = allButtons[i];
      const text = await button.textContent();
      const isVisible = await button.isVisible();
      console.log(
        `  ${i + 1}. Text: "${text?.trim() || "N/A"}" | Visible: ${isVisible}`
      );
    }

    // Check for any headings
    const allHeadings = await page.locator("h1, h2, h3, h4, h5, h6").all();
    console.log(`\n📚 Found ${allHeadings.length} headings:`);

    for (let i = 0; i < allHeadings.length; i++) {
      const heading = allHeadings[i];
      const text = await heading.textContent();
      const tagName = await heading.evaluate((el) => el.tagName.toLowerCase());
      const isVisible = await heading.isVisible();
      console.log(
        `  ${i + 1}. <${tagName}>: "${
          text?.trim() || "N/A"
        }" | Visible: ${isVisible}`
      );
    }

    // Check for any divs with text
    const allDivs = await page.locator("div").all();
    console.log(
      `\n📦 Found ${allDivs.length} divs (showing only those with text):`
    );

    let divCount = 0;
    for (let i = 0; i < allDivs.length && divCount < 20; i++) {
      const div = allDivs[i];
      const text = await div.textContent();
      if (text && text.trim().length > 0) {
        divCount++;
        const isVisible = await div.isVisible();
        console.log(
          `  ${divCount}. Text: "${text
            .trim()
            .substring(0, 100)}..." | Visible: ${isVisible}`
        );
      }
    }

    // Take a screenshot for debugging
    await page.screenshot({ path: "page-debug.png", fullPage: true });
    console.log("📸 Screenshot saved as page-debug.png");

    // Check console for any errors
    const consoleLogs = [];
    page.on("console", (msg) => {
      consoleLogs.push(msg);
      console.log(`🔧 Console [${msg.type()}]: ${msg.text()}`);
    });

    // Wait a bit more to capture console logs
    await page.waitForTimeout(1000);
  } catch (error) {
    console.error("❌ Error:", error.message);
    console.error("Stack:", error.stack);
  } finally {
    await browser.close();
  }
}

inspectPage();
