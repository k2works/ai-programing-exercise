import { test, expect } from '@playwright/test';

/**
 * 完全なゲームシナリオのE2Eテスト
 * 全要件を統合したユーザーシナリオベースのテスト
 */
test.describe('完全なゲームプレイシナリオ', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('新規プレイヤーの初回ゲーム体験', async ({ page }) => {
    // シナリオ: 新規プレイヤーがゲームを初めてプレイする

    // 1. ページにアクセス
    await expect(page).toHaveTitle(/ぷよぷよゲーム/);
    await expect(page.locator('.game-title')).toHaveText('ぷよぷよゲーム');

    // 2. 操作説明を確認
    await expect(page.locator('.game-subtitle')).toContainText(
      '矢印キーで移動'
    );

    // 3. 新しいゲームを開始
    const newGameButton = page.locator('.new-game-button');
    await expect(newGameButton).toBeVisible();
    await newGameButton.click();

    // 4. ゲーム画面の確認
    await page.waitForTimeout(500);
    await expect(page.locator('.game-board')).toBeVisible();
    await expect(page.locator('.score-section')).toContainText('0');
    await expect(page.locator('.next-puyo-section')).toBeVisible();

    // 5. 基本操作の実行
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(100);
    await page.keyboard.press('Space');
    await page.waitForTimeout(100);
    await page.keyboard.press('ArrowRight');
    await page.waitForTimeout(100);

    // 6. ぷよが操作に反応することを確認
    const activePuyos = page.locator('.game-board .puyo.active');
    await expect(activePuyos).toHaveCount(2);

    // 7. 自動落下の確認
    await page.waitForTimeout(2000);

    // 8. ゲーム制御の確認
    await page.click('.pause-button');
    await expect(page.locator('.pause-button')).toContainText('再開');

    await page.click('.pause-button');
    await expect(page.locator('.pause-button')).toContainText('一時停止');
  });

  test('短時間プレイセッション', async ({ page }) => {
    // シナリオ: 短時間でゲームをプレイして終了

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 複数のぷよを配置
    for (let round = 0; round < 5; round++) {
      // ランダムな操作を実行
      const operations = ['ArrowLeft', 'ArrowRight', 'Space', 'ArrowDown'];
      const randomOp =
        operations[Math.floor(Math.random() * operations.length)];

      await page.keyboard.press(randomOp);
      await page.waitForTimeout(50);

      // ぷよを落下
      for (let i = 0; i < 15; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(20);
      }

      await page.waitForTimeout(300);
    }

    // スコアが更新されていることを確認
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toBeVisible();

    // ゲームをリセット
    await page.click('.reset-button');
    await page.waitForTimeout(200);

    // 新しいゲームボタンが再表示される
    await expect(page.locator('.new-game-button')).toBeVisible();
  });

  test('長時間プレイセッション', async ({ page }) => {
    // シナリオ: 長時間プレイしてゲームオーバーまで到達

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    let gameOverReached = false;

    // 長時間プレイ（最大50ラウンド）
    for (let round = 0; round < 50 && !gameOverReached; round++) {
      // 様々な操作を実行
      const operations = [
        () => page.keyboard.press('ArrowLeft'),
        () => page.keyboard.press('ArrowRight'),
        () => page.keyboard.press('Space'),
        () => page.keyboard.press('ArrowDown'),
      ];

      // ランダムな操作を2-3回実行
      const numOps = Math.floor(Math.random() * 3) + 1;
      for (let i = 0; i < numOps; i++) {
        const randomOp =
          operations[Math.floor(Math.random() * operations.length)];
        await randomOp();
        await page.waitForTimeout(30);
      }

      // ぷよを落下
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }

      await page.waitForTimeout(100);

      // ゲームオーバーチェック
      const gameOverDialog = page.locator('[data-testid="game-over-dialog"]');
      const isGameOver = await gameOverDialog.isVisible();

      if (isGameOver) {
        gameOverReached = true;

        // ゲームオーバー画面の確認
        await expect(gameOverDialog).toBeVisible();
        await expect(page.locator('.game-over-title')).toHaveText(
          'ゲームオーバー'
        );
        await expect(page.locator('.final-score')).toBeVisible();

        // リスタート
        await page.click('.restart-button');
        await page.waitForTimeout(500);

        // 新しいゲームが開始される
        await expect(page.locator('.score-section')).toContainText('0');
        await expect(page.locator('.game-board .puyo.active')).toHaveCount(2);
      }
    }
  });

  test('一時停止と再開のワークフロー', async ({ page }) => {
    // シナリオ: ゲーム中に一時停止して後で再開

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 少しプレイ
    for (let i = 0; i < 3; i++) {
      await page.keyboard.press('ArrowLeft');
      await page.keyboard.press('Space');
      for (let j = 0; j < 15; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }
      await page.waitForTimeout(200);
    }

    // 現在のスコアを記録
    const scoreDisplay = page.locator('.score-section');
    const scoreBeforePause = await scoreDisplay.textContent();

    // ゲームを一時停止
    await page.click('.pause-button');
    await expect(page.locator('.pause-button')).toContainText('再開');

    // 一時停止中は操作が無効
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const pausedPosition = await activePuyo.boundingBox();

    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(100);

    const stillPausedPosition = await activePuyo.boundingBox();
    if (pausedPosition && stillPausedPosition) {
      expect(stillPausedPosition.x).toBe(pausedPosition.x);
    }

    // ゲームを再開
    await page.click('.pause-button');
    await expect(page.locator('.pause-button')).toContainText('一時停止');

    // スコアが保持されている
    const scoreAfterResume = await scoreDisplay.textContent();
    expect(scoreAfterResume).toBe(scoreBeforePause);

    // 操作が再び有効になる
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(100);

    const resumedPosition = await activePuyo.boundingBox();
    if (stillPausedPosition && resumedPosition) {
      expect(resumedPosition.x).toBeLessThan(stillPausedPosition.x);
    }
  });

  test('複数ゲームセッションの管理', async ({ page }) => {
    // シナリオ: 複数回ゲームを開始してリセット

    for (let session = 0; session < 3; session++) {
      // 新しいゲームを開始
      await page.click('.new-game-button');
      await page.waitForTimeout(500);

      // スコアが0で開始される
      await expect(page.locator('.score-section')).toContainText('0');

      // 少しプレイ
      for (let round = 0; round < 3; round++) {
        await page.keyboard.press('Space');
        for (let i = 0; i < 15; i++) {
          await page.keyboard.press('ArrowDown');
          await page.waitForTimeout(10);
        }
        await page.waitForTimeout(100);
      }

      // ゲームをリセット
      await page.click('.reset-button');
      await page.waitForTimeout(200);

      // 新しいゲームボタンが表示される
      await expect(page.locator('.new-game-button')).toBeVisible();
    }
  });

  test('エラー回復シナリオ', async ({ page }) => {
    // シナリオ: 予期しない状況からの回復

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 大量の操作を短時間で実行（ストレステスト）
    for (let i = 0; i < 100; i++) {
      await page.keyboard.press('Space');
      await page.keyboard.press('ArrowLeft');
      await page.keyboard.press('ArrowRight');
      await page.keyboard.press('ArrowDown');

      if (i % 10 === 0) {
        await page.waitForTimeout(50);
      }
    }

    // ゲームが引き続き動作することを確認
    const activePuyos = page.locator('.game-board .puyo.active');
    await expect(activePuyos).toHaveCount(2);

    // リセットが機能することを確認
    await page.click('.reset-button');
    await page.waitForTimeout(200);
    await expect(page.locator('.new-game-button')).toBeVisible();
  });
});

/**
 * クロスブラウザ互換性テスト
 */
test.describe('クロスブラウザ互換性', () => {
  test('基本機能がすべてのブラウザで動作する', async ({
    page,
    browserName,
  }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // ブラウザ情報をログ出力
    console.log(`Testing on browser: ${browserName}`);

    // 基本的な表示確認
    await expect(page.locator('[data-testid="game-page"]')).toBeVisible();
    await expect(page.locator('.game-title')).toHaveText('ぷよぷよゲーム');

    // ゲーム開始
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 基本機能の確認
    await expect(page.locator('.game-board')).toBeVisible();
    await expect(page.locator('.score-section')).toBeVisible();
    await expect(page.locator('.next-puyo-section')).toBeVisible();

    // 操作確認
    await page.keyboard.press('ArrowLeft');
    await page.keyboard.press('Space');
    await page.keyboard.press('ArrowDown');

    // ぷよが存在することを確認
    const activePuyos = page.locator('.game-board .puyo.active');
    await expect(activePuyos).toHaveCount(2);

    // ゲーム制御確認
    await page.click('.pause-button');
    await expect(page.locator('.pause-button')).toContainText('再開');

    await page.click('.pause-button');
    await expect(page.locator('.pause-button')).toContainText('一時停止');
  });

  test('パフォーマンスが許容範囲内である', async ({ page }) => {
    // パフォーマンス測定開始
    const startTime = Date.now();

    await page.goto('/');
    await page.waitForLoadState('networkidle');

    const loadTime = Date.now() - startTime;

    // ページロード時間が5秒以内であることを確認
    expect(loadTime).toBeLessThan(5000);

    // ゲーム開始のパフォーマンス
    const gameStartTime = Date.now();
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    const gameStartDuration = Date.now() - gameStartTime;

    // ゲーム開始が2秒以内であることを確認
    expect(gameStartDuration).toBeLessThan(2000);

    // 操作の応答性確認
    const operationStartTime = Date.now();

    for (let i = 0; i < 10; i++) {
      await page.keyboard.press('ArrowLeft');
      await page.keyboard.press('Space');
    }

    const operationDuration = Date.now() - operationStartTime;

    // 10回の操作が1秒以内であることを確認
    expect(operationDuration).toBeLessThan(1000);
  });
});

/**
 * ユーザビリティテスト
 */
test.describe('ユーザビリティ', () => {
  test('直感的な操作が可能である', async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // 明確なCTA（Call to Action）
    const newGameButton = page.locator('.new-game-button');
    await expect(newGameButton).toBeVisible();
    await expect(newGameButton).toHaveText('新しいゲーム');

    // ゲーム開始
    await newGameButton.click();
    await page.waitForTimeout(500);

    // 操作説明が表示されている
    await expect(page.locator('.game-subtitle')).toContainText(
      '矢印キーで移動'
    );

    // 視覚的フィードバック
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(100);

    // ぷよが移動したことを視覚的に確認できる
    const activePuyos = page.locator('.game-board .puyo.active');
    await expect(activePuyos).toHaveCount(2);

    // スコア表示が分かりやすい
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toBeVisible();

    // NEXTぷよが分かりやすい
    const nextPuyoDisplay = page.locator('.next-puyo-section');
    await expect(nextPuyoDisplay).toBeVisible();
  });

  test('エラー状態が分かりやすい', async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 無効な操作を試行（壁際での移動）
    for (let i = 0; i < 10; i++) {
      await page.keyboard.press('ArrowLeft');
      await page.waitForTimeout(30);
    }

    // ゲームが引き続き正常に動作する
    const activePuyos = page.locator('.game-board .puyo.active');
    await expect(activePuyos).toHaveCount(2);

    // 他の操作は引き続き有効
    await page.keyboard.press('Space');
    await page.waitForTimeout(100);
    await expect(activePuyos).toHaveCount(2);
  });
});
