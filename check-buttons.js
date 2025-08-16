const { chromium } = require('@playwright/test');

async function checkButtons() {
  const browser = await chromium.launch({ headless: false });
  const page = await browser.newPage();

  try {
    // Navigate to your app
    await page.goto('http://localhost:8889');
    console.log('✅ Page loaded successfully');

    // Wait for the page to fully load
    await page.waitForLoadState('networkidle');

    // Check for the "Mannschaft erstellen" link
    const createTeamLink = page.getByRole('link', { name: 'Mannschaft erstellen' });
    if (await createTeamLink.isVisible()) {
      console.log('✅ Found "Mannschaft erstellen" link');
    } else {
      console.log('❌ "Mannschaft erstellen" link not found');
    }

    // Click on create team link
    await createTeamLink.click();
    console.log('✅ Clicked on create team link');

    // Wait for the create team page to load
    await page.waitForURL('**/create');
    console.log('✅ Navigated to create team page');

    // Check for the heading
    const heading = page.getByRole('heading', { name: 'Neue Mannschaft erstellen' });
    if (await heading.isVisible()) {
      console.log('✅ Found heading "Neue Mannschaft erstellen"');
    } else {
      console.log('❌ Heading "Neue Mannschaft erstellen" not found');
    }

    // Check for the submit button
    const submitButton = page.getByRole('button', { name: 'Mannschaft erstellen' });
    if (await submitButton.isVisible()) {
      console.log('✅ Found submit button "Mannschaft erstellen"');
    } else {
      console.log('❌ Submit button "Mannschaft erstellen" not found');
    }

    // List all buttons on the page
    const allButtons = await page.locator('button').all();
    console.log(`\n📋 Found ${allButtons.length} buttons on the page:`);

    for (let i = 0; i < allButtons.length; i++) {
      const button = allButtons[i];
      const text = await button.textContent();
      const ariaLabel = await button.getAttribute('aria-label');
      console.log(`  ${i + 1}. Text: "${text?.trim() || 'N/A'}" | Aria-label: "${ariaLabel || 'N/A'}"`);
    }

    // List all links on the page
    const allLinks = await page.locator('a').all();
    console.log(`\n🔗 Found ${allLinks.length} links on the page:`);

    for (let i = 0; i < allLinks.length; i++) {
      const link = allLinks[i];
      const text = await link.textContent();
      const href = await link.getAttribute('href');
      console.log(`  ${i + 1}. Text: "${text?.trim() || 'N/A'}" | Href: "${href || 'N/A'}"`);
    }

  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    await browser.close();
  }
}

checkButtons();


