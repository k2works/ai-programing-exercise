import { test, expect } from '@playwright/test';

test('アプリケーションが正常に表示される', async ({ page }) => {
  await page.goto('/');

  // ページタイトルを確認
  await expect(page).toHaveTitle(/ぷよぷよゲーム/);
});

test('基本的なページ要素が表示される', async ({ page }) => {
  await page.goto('/');

  // ページが読み込まれることを確認
  await expect(page.locator('body')).toBeVisible();
});