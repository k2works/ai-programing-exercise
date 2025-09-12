# React Job Board Application - AI Programming Exercise

## 概要

React Job Board Application は求人掲載・管理・応募システムであり、求職者・採用担当者・組織管理者の3つの役割に対応したWebアプリケーションです。

### 目的

- **React アプリケーション アーキテクチャ** の実践的実装
- **XP（エクストリーム プログラミング）** 手法による開発
- **CI/CD パイプライン** の構築と運用
- **テスト駆動開発（TDD）** の実践

### 技術スタック

#### フロントエンド
- **Framework**: Next.js 14.2+ (Pages Router)
- **Language**: TypeScript 5.0+
- **State Management**: Zustand + React Query
- **UI Library**: Tailwind CSS + React Hook Form
- **Testing**: Jest + React Testing Library + Cypress
- **Build**: Webpack 5 + SWC

#### バックエンド  
- **Framework**: .NET 9.0 Web API
- **Language**: C# 12.0
- **Database**: SQLite (開発) / PostgreSQL (本番)
- **Authentication**: JWT
- **Testing**: xUnit + NSubstitute

#### 開発・運用
- **CI/CD**: GitHub Actions
- **Deployment**: Vercel (Frontend) + Railway/Heroku (Backend)
- **Monitoring**: Lighthouse CI
- **Package Manager**: npm (Frontend) / NuGet (Backend)

---

## プロジェクト構造

```
ai-programing-exercise/
├── app/
│   ├── frontend/                 # Next.js React アプリケーション
│   │   ├── src/
│   │   │   ├── components/       # 再利用可能なコンポーネント
│   │   │   ├── features/         # 機能別モジュール
│   │   │   ├── pages/           # Next.js ページコンポーネント
│   │   │   ├── hooks/           # カスタムフック
│   │   │   ├── stores/          # Zustand ストア
│   │   │   ├── types/           # TypeScript 型定義
│   │   │   └── testing/         # テスト用ヘルパー
│   │   ├── cypress/             # E2E テスト
│   │   ├── next.config.js       # Next.js 設定
│   │   ├── jest.config.js       # Jest 設定
│   │   └── package.json
│   └── backend/                 # .NET Web API (実装予定)
│       ├── JobBoard.Api/        # API プロジェクト
│       ├── JobBoard.Core/       # ビジネスロジック
│       └── JobBoard.Tests/      # テストプロジェクト
├── docs/                        # ドキュメント
│   ├── development/             # 開発計画・仕様
│   ├── reference/              # 技術リファレンス
│   └── requirements/           # 要件定義
├── .github/workflows/          # CI/CD パイプライン
├── .env.example               # 環境変数テンプレート
├── vercel.json               # Vercel デプロイ設定
└── README.md
```

---

## セットアップ

### 前提条件

| ツール | バージョン | 用途 |
|--------|-----------|------|
| Node.js | 22.x | フロントエンド実行環境 |
| npm | 10.x | パッケージ管理 |
| .NET SDK | 9.0 | バックエンド実行環境 |
| Git | 最新 | バージョン管理 |

### インストール

1. **リポジトリのクローン**
   ```bash
   git clone https://github.com/your-username/ai-programing-exercise.git
   cd ai-programing-exercise
   ```

2. **環境変数の設定**
   ```bash
   cp .env.example .env
   # 必要に応じて .env ファイルを編集
   ```

3. **フロントエンドのセットアップ**
   ```bash
   cd app/frontend
   npm install
   ```

4. **バックエンドのセットアップ** (実装後)
   ```bash
   cd app/backend
   dotnet restore
   ```

### 開発サーバーの起動

#### フロントエンド
```bash
cd app/frontend
npm run dev
# http://localhost:3000 でアクセス
```

#### バックエンド (実装後)
```bash
cd app/backend
dotnet run
# http://localhost:5150/api でアクセス
```

---

## 開発ガイド

### XP（エクストリーム プログラミング）実践

本プロジェクトは XP の価値とプラクティスに基づいて開発されます：

**XP の価値**:
- **コミュニケーション**: オープンで正直な対話
- **シンプリシティ**: 最もシンプルな解決策を追求
- **フィードバック**: 迅速で頻繁なフィードバックループ
- **勇気**: 変更と改善への積極的な姿勢

**主要プラクティス**:
- **テスト駆動開発（TDD）**: Red-Green-Refactorサイクル
- **ペアプログラミング**: 知識共有と品質向上
- **継続的インテグレーション**: 頻繁なコード統合
- **リファクタリング**: 継続的なコード改善
- **小さなリリース**: 2週間イテレーションでの価値提供

### テスト戦略

#### テストピラミッド
- **ユニットテスト**: ビジネスロジック・バリデーション (90%以上)
- **統合テスト**: API・コンポーネント連携 (中程度)
- **E2Eテスト**: ユーザーシナリオ (最小限・重要パス)

#### フロントエンドテスト
```bash
# ユニット・統合テスト実行
npm test

# カバレッジ付きテスト
npm run test:coverage

# E2Eテスト実行
npm run cypress:run

# E2Eテスト（インタラクティブ）
npm run cypress:open
```

#### バックエンドテスト (実装後)
```bash
# テスト実行
dotnet test

# カバレッジ付きテスト
dotnet test --collect:"XPlat Code Coverage"
```

### コード品質

#### ESLint・Prettier
```bash
# リント実行
npm run lint

# フォーマット実行
npm run format

# 型チェック
npm run types:check
```

#### コミット前チェック
Git pre-commit hooks により以下が自動実行されます：
- ESLint による静的解析
- Prettier によるフォーマット
- TypeScript 型チェック
- ユニットテスト実行

---

## CI/CD パイプライン

### GitHub Actions ワークフロー

#### code-checks
- **トリガー**: プッシュ・プルリクエスト
- **実行内容**: 
  - TypeScript型チェック
  - ESLint
  - Prettier フォーマットチェック  
  - Jest ユニットテスト
  - カバレッジレポート

#### security-scan
- **トリガー**: プッシュ・プルリクエスト
- **実行内容**:
  - npm audit による脆弱性チェック
  - TruffleHog によるシークレット検出

#### e2e-tests
- **トリガー**: main/develop ブランチへのプッシュ
- **実行内容**: Cypress E2E テスト実行

#### build-test
- **トリガー**: コードチェック完了後
- **実行内容**: 
  - 開発・本番環境でのビルド確認
  - バンドルサイズチェック

#### deploy-preview
- **トリガー**: プルリクエスト作成時
- **実行内容**: Vercel プレビュー環境デプロイ

#### deploy-production
- **トリガー**: main ブランチへのマージ
- **実行内容**: Vercel 本番環境デプロイ

#### performance-audit  
- **トリガー**: 本番デプロイ完了後
- **実行内容**: Lighthouse パフォーマンス監査

### 品質ゲート

全デプロイメントで以下の品質基準をクリアする必要があります：

| 項目 | 基準値 |
|------|--------|
| テストカバレッジ | 80%以上 |
| TypeScript型エラー | 0件 |
| ESLint エラー | 0件 |
| セキュリティ脆弱性 | 高・重大 0件 |
| Lighthouse Performance | 80点以上 |
| Lighthouse Accessibility | 90点以上 |

---

## デプロイメント

### 環境構成

| 環境 | URL | 用途 |
|------|-----|------|
| Development | http://localhost:3000 | ローカル開発 |
| Preview | https://preview-{PR}.vercel.app | PR レビュー用 |
| Production | https://job-board.vercel.app | 本番環境 |

### 環境変数

各環境で以下の環境変数を設定：

```bash
# 認証設定
NEXTAUTH_URL=https://your-domain.com
NEXTAUTH_SECRET=your-secret-key

# API設定
NEXT_PUBLIC_API_URL=https://api.your-domain.com/api
NEXT_PUBLIC_ENABLE_API_MOCKING=false

# 機能フラグ
NEXT_PUBLIC_ENABLE_ANALYTICS=true
NEXT_PUBLIC_ENABLE_NOTIFICATIONS=true

# セキュリティ
JWT_SECRET=your-jwt-secret
```

### デプロイ手順

#### Vercel への手動デプロイ
```bash
# Vercel CLI インストール
npm install -g vercel

# プロジェクトリンク
cd app/frontend
vercel

# 本番デプロイ
vercel --prod
```

#### Railway への Backend デプロイ (実装後)
```bash
# Railway CLI インストール
npm install -g @railway/cli

# プロジェクト初期化
railway login
railway init

# デプロイ
railway up
```

---

## モニタリング・運用

### パフォーマンス監視

- **Lighthouse CI**: 継続的パフォーマンス監視
- **Core Web Vitals**: ユーザー体験指標追跡
- **Bundle Analyzer**: バンドルサイズ最適化

### エラー追跡

- **React Error Boundary**: フロントエンドエラー処理
- **Sentry**: エラーレポート（オプション）
- **Console ログ**: 開発環境デバッグ

### アクセシビリティ

- **cypress-axe**: 自動アクセシビリティテスト
- **ESLint a11y**: 静的解析
- **WCAG 2.1 AA**: 準拠レベル目標

---

## 貢献方法

### ブランチ戦略

```
main (本番)
├── develop (開発統合)
├── feature/US001-job-search (機能開発)
├── feature/US005-user-registration (機能開発)
└── hotfix/critical-bug (緊急修正)
```

### プルリクエスト

1. **ブランチ作成**: `feature/US001-description`
2. **実装**: TDD サイクルに従った開発
3. **テスト**: 全テスト成功確認
4. **プルリクエスト作成**: テンプレートに従った記述
5. **レビュー**: 2人以上の承認
6. **マージ**: Squash and merge

### コミットメッセージ

```bash
# 形式: type(scope): description
feat(auth): ユーザー登録機能を追加
fix(search): 検索結果の表示バグを修正
test(jobs): 求人作成のE2Eテストを追加
docs(readme): セットアップ手順を更新
```

---

## トラブルシューティング

### よくある問題

#### ビルドエラー
```bash
# 依存関係の再インストール
rm -rf node_modules package-lock.json
npm install

# TypeScript型エラー
npm run types:check
```

#### テスト失敗
```bash
# テストキャッシュクリア
npm test -- --clearCache

# 特定テスト実行
npm test -- --testNamePattern="User Login"
```

#### Cypress テスト
```bash
# Cypress バイナリ再インストール
npx cypress install

# ヘッドレスモードで実行
npm run cypress:run
```

### ログ確認

#### フロントエンド
```bash
# 開発サーバーログ
npm run dev

# ビルドログ
npm run build
```

#### バックエンド (実装後)
```bash
# アプリケーションログ
dotnet run --verbosity detailed

# テストログ
dotnet test --logger console --verbosity normal
```

---

## ライセンス

このプロジェクトは MIT ライセンスの下で公開されています。詳細は [LICENSE](LICENSE) ファイルを参照してください。

---

## 関連リンク

- **プロジェクト Wiki**: 詳細な技術仕様
- **API ドキュメント**: Swagger/OpenAPI 仕様書
- **デザインシステム**: Storybook コンポーネントライブラリ  
- **パフォーマンス監視**: Lighthouse CI レポート

---

**最終更新**: 2025-09-12  
**メンテナー**: AI Programming Exercise Team