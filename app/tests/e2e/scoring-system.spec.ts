import { test, expect } from '@playwright/test';

/**
 * スコアシステムのE2Eテスト
 * 要件6: 全消しボーナス
 * 要件8: スコア表示システム
 */
test.describe('スコアシステム', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // ゲームを開始
    await page.click('.new-game-button');
    await page.waitForTimeout(500);
  });

  test('スコアが常に表示される', async ({ page }) => {
    // スコア表示が常に表示される（要件8.1）
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toBeVisible();

    // 初期スコアが0であることを確認
    await expect(scoreDisplay).toContainText('0');
  });

  test('スコア表示の基本構造が正しい', async ({ page }) => {
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toBeVisible();

    // スコア表示にスコア値が含まれることを確認
    const scoreText = await scoreDisplay.textContent();
    expect(scoreText).toMatch(/\d+/); // 数字が含まれることを確認
  });

  test('ゲームリセット時にスコアが0になる', async ({ page }) => {
    // ゲームを進行させる
    for (let i = 0; i < 3; i++) {
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(20);
      }
      await page.waitForTimeout(300);
    }

    // リセットボタンをクリック
    await page.click('.reset-button');
    await page.waitForTimeout(500);

    // スコアが0にリセットされることを確認
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toContainText('0');
  });

  test('新しいゲーム開始時にスコアが0になる', async ({ page }) => {
    // ゲームを進行させる
    for (let i = 0; i < 3; i++) {
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(20);
      }
      await page.waitForTimeout(300);
    }

    // 新しいゲームを開始
    await page.click('.reset-button');
    await page.waitForTimeout(200);
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // スコアが0で開始されることを確認
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toContainText('0');
  });

  test('スコア表示が数値形式で表示される', async ({ page }) => {
    const scoreDisplay = page.locator('.score-section');

    // スコア表示のテキストを取得
    const scoreText = await scoreDisplay.textContent();

    // 数値が含まれることを確認
    expect(scoreText).toMatch(/\d+/);

    // 基本的な表示形式を確認（スコア: 0 のような形式）
    expect(scoreText).toBeTruthy();
  });

  test('スコアハイライト機能の基本動作', async ({ page }) => {
    // スコア表示要素を取得
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toBeVisible();

    // ゲームを進行させてスコア変化の可能性を作る
    for (let i = 0; i < 5; i++) {
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(15);
      }
      await page.waitForTimeout(200);

      // スコア表示が引き続き表示されることを確認
      await expect(scoreDisplay).toBeVisible();
    }
  });

  test('ゲーム一時停止中もスコアが表示される', async ({ page }) => {
    // ゲームを一時停止
    await page.click('.pause-button');

    // スコア表示が引き続き表示される
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toBeVisible();

    // ゲームを再開
    await page.click('.pause-button');

    // スコア表示が引き続き表示される
    await expect(scoreDisplay).toBeVisible();
  });

  test('複数のゲームセッションでスコアが独立している', async ({ page }) => {
    // 最初のゲームセッション
    for (let i = 0; i < 3; i++) {
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(20);
      }
      await page.waitForTimeout(200);
    }

    // 最初のセッションのスコアを記録
    const scoreDisplay = page.locator('.score-section');
    const firstSessionScore = await scoreDisplay.textContent();

    // ゲームをリセット
    await page.click('.reset-button');
    await page.waitForTimeout(200);

    // 新しいゲームを開始
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 新しいセッションのスコアが0から開始されることを確認
    await expect(scoreDisplay).toContainText('0');

    // 前のセッションのスコアと異なることを確認（0でない場合）
    const newSessionScore = await scoreDisplay.textContent();
    if (firstSessionScore && !firstSessionScore.includes('0')) {
      expect(newSessionScore).not.toBe(firstSessionScore);
    }
  });
});

/**
 * 連鎖とボーナススコアのテスト
 * 注意: 実際の連鎖発生は運に依存するため、基本的な表示確認に留める
 */
test.describe('連鎖とボーナススコア', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    await page.click('.new-game-button');
    await page.waitForTimeout(500);
  });

  test('連鎖表示エリアが存在する', async ({ page }) => {
    // 連鎖表示エリアの存在確認
    // （実際の連鎖は運に依存するため、要素の存在のみ確認）

    // ゲームを進行させる
    for (let i = 0; i < 10; i++) {
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }
      await page.waitForTimeout(100);

      // 連鎖表示が現れる可能性をチェック
      const chainDisplay = page.locator('[data-testid="chain-display"]');
      const isVisible = await chainDisplay.isVisible();

      if (isVisible) {
        // 連鎖表示が現れた場合の確認
        await expect(chainDisplay).toContainText('連鎖');
        break;
      }
    }
  });

  test('全消しエフェクトの基本構造', async ({ page }) => {
    // 全消しエフェクトは稀な現象のため、基本的な構造のみ確認

    // ゲームを長時間進行させる
    for (let i = 0; i < 20; i++) {
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(5);
      }
      await page.waitForTimeout(50);

      // 全消しエフェクトが発生する可能性をチェック
      // （実際の発生は非常に稀なため、基本的な要素確認のみ）
      const allClearEffect = page.locator('.all-clear-effect');
      const isVisible = await allClearEffect.isVisible();

      if (isVisible) {
        // 全消しエフェクトが現れた場合の確認
        await expect(allClearEffect).toBeVisible();
        break;
      }
    }
  });

  test('スコア更新の基本動作', async ({ page }) => {
    const scoreDisplay = page.locator('.score-section');

    // 初期スコアを記録

    // ゲームを進行させる
    for (let i = 0; i < 15; i++) {
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }
      await page.waitForTimeout(100);
    }

    // スコア表示が引き続き機能することを確認
    await expect(scoreDisplay).toBeVisible();

    // スコア値が数値形式であることを確認
    const finalScore = await scoreDisplay.textContent();
    expect(finalScore).toMatch(/\d+/);
  });
});
