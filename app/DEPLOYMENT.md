# デプロイメントガイド

## プロダクション環境への準備

### 1. 品質チェック

#### コード品質

- [ ] ESLintエラーなし (`npm run lint`)
- [ ] Prettierフォーマット適用済み (`npm run format:check`)
- [ ] TypeScriptコンパイルエラーなし (`tsc --noEmit`)

#### テスト

- [ ] 全単体テスト通過 (`npm run test`)
- [ ] テストカバレッジ80%以上 (`npm run test:coverage`)
- [ ] E2Eテスト通過 (`npm run test:e2e`)

#### パフォーマンス

- [ ] プロダクションビルド成功 (`npm run build`)
- [ ] バンドルサイズ警告なし
- [ ] Lighthouse監査スコア80%以上

### 2. セキュリティチェック

#### 依存関係

- [ ] 脆弱性なし (`npm audit`)
- [ ] 依存関係最新化 (`npm update`)

#### セキュリティヘッダー

- [ ] Content Security Policy設定済み
- [ ] X-Frame-Options設定済み
- [ ] X-Content-Type-Options設定済み

### 3. デプロイメント設定

#### Vercel設定

- [ ] `vercel.json`設定完了
- [ ] 環境変数設定（必要に応じて）
- [ ] ドメイン設定

#### CI/CD設定

- [ ] GitHub Actions設定完了
- [ ] シークレット変数設定
- [ ] デプロイメント自動化

### 4. 監視・ログ設定

#### パフォーマンス監視

- [ ] Vercel Analytics設定
- [ ] Core Web Vitals監視
- [ ] エラー追跡設定

#### ログ設定

- [ ] エラーログ設定
- [ ] アクセスログ設定
- [ ] パフォーマンスログ設定

## デプロイメント手順

### 自動デプロイ（推奨）

1. **mainブランチへのプッシュ**

   ```bash
   git checkout main
   git merge develop
   git push origin main
   ```

2. **GitHub Actionsによる自動実行**
   - テスト実行
   - ビルド
   - デプロイ
   - パフォーマンス監査

### 手動デプロイ

1. **ローカルビルド確認**

   ```bash
   npm run check:full
   ```

2. **Vercelデプロイ**
   ```bash
   npx vercel --prod
   ```

## デプロイ後の確認

### 機能確認

- [ ] ゲーム開始機能
- [ ] ぷよ操作機能
- [ ] スコア表示機能
- [ ] レスポンシブデザイン
- [ ] アクセシビリティ

### パフォーマンス確認

- [ ] 初期ロード時間 < 3秒
- [ ] First Contentful Paint < 2秒
- [ ] Largest Contentful Paint < 3秒
- [ ] Cumulative Layout Shift < 0.1

### セキュリティ確認

- [ ] HTTPS接続
- [ ] セキュリティヘッダー
- [ ] CSP違反なし

## ロールバック手順

### 緊急時のロールバック

1. **Vercelダッシュボードから**
   - 前のデプロイメントを選択
   - "Promote to Production"をクリック

2. **CLIから**
   ```bash
   vercel rollback [deployment-url]
   ```

### Gitレベルでのロールバック

1. **コミットの取り消し**
   ```bash
   git revert [commit-hash]
   git push origin main
   ```

## トラブルシューティング

### よくある問題

#### ビルドエラー

- TypeScriptエラー確認
- 依存関係の問題確認
- メモリ不足の確認

#### デプロイエラー

- Vercel設定確認
- 環境変数確認
- ファイルサイズ制限確認

#### パフォーマンス問題

- バンドルサイズ確認
- 不要な依存関係削除
- コード分割の最適化

### ログ確認方法

#### Vercelログ

```bash
vercel logs [deployment-url]
```

#### ローカルデバッグ

```bash
npm run build
npm run preview
```

## 環境変数

### 本番環境で必要な環境変数

```bash
# Vercel設定
VERCEL_TOKEN=your_vercel_token
VERCEL_ORG_ID=your_org_id
VERCEL_PROJECT_ID=your_project_id

# 監視・分析（オプション）
VERCEL_ANALYTICS_ID=your_analytics_id
```

## パフォーマンス最適化

### 実装済み最適化

- React.memo使用
- useMemo/useCallback使用
- バンドル分割
- 静的アセット最適化
- Terser圧縮

### 追加最適化案

- Service Worker実装
- 画像最適化
- フォント最適化
- プリロード設定

## 監視とメンテナンス

### 定期チェック項目

- [ ] 依存関係更新（月次）
- [ ] セキュリティ監査（月次）
- [ ] パフォーマンス監査（週次）
- [ ] エラーログ確認（日次）

### アラート設定

- エラー率閾値
- レスポンス時間閾値
- 可用性閾値
- Core Web Vitals閾値
