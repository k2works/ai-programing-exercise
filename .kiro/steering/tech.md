---
inclusion: always
---

# 技術スタック & 開発規約

## 必須技術スタック

### コア技術
- **TypeScript 5.0+**: 全てのコードはTypeScriptで記述する
- **React 18+**: 関数型コンポーネントとHooksを使用
- **Node.js 18+**: 開発環境とビルドツール
- **Vite**: ビルドツールとして使用

### 状態管理
- **Zustand**: 軽量でシンプルな状態管理ライブラリを使用
- Redux Toolkitは使用しない（複雑すぎるため）

### スタイリング
- **Tailwind CSS**: ユーティリティファーストのCSSフレームワーク
- **CSS Modules**: コンポーネント固有のスタイル
- **Framer Motion**: アニメーション実装
- Styled Componentsは使用しない（パフォーマンス懸念）

### AI・機械学習
- **TensorFlow.js**: ブラウザ内機械学習とゲームAI実装

### 開発ツール
- **npm**: パッケージマネージャー（yarnやpnpmは使用しない）
- **ESLint + Prettier**: コード品質とフォーマット
- **Vitest**: テストフレームワーク（Jest互換API）
- **React Testing Library**: コンポーネントテスト
- **Playwright**: E2Eテスト

## コーディング規約

### TypeScript規約
- 厳格な型チェックを有効にする（strict: true）
- `any`型の使用を避ける
- インターフェースよりもtype aliasを優先
- 関数の戻り値型を明示的に指定
- nullableな値には適切なガード句を使用

### React規約
- 関数型コンポーネントのみ使用（クラスコンポーネント禁止）
- カスタムHooksで状態ロジックを分離
- propsの型定義を必須とする
- useEffectの依存配列を適切に管理
- コンポーネント名はPascalCaseで命名

### ファイル構成規約
- コンポーネントファイルは`.tsx`拡張子
- ユーティリティファイルは`.ts`拡張子
- テストファイルは`.test.tsx`または`.test.ts`
- ファイル名はkebab-caseまたはPascalCase（コンポーネント）

## テスト規約

### テスト戦略
- 単体テスト: Vitest + React Testing Library
- E2Eテスト: Playwright
- カバレッジ: c8（最低80%を目標）
- モック: Vitest内蔵のvi関数を使用

### テスト命名規則
- テストファイル: `ComponentName.test.tsx`
- テスト関数: `describe`と`it`を使用
- テスト名は日本語で記述可能
- Given-When-Then形式でテストを構造化

## UI・スタイリング規約

### スタイリング規則
- Tailwind CSSのユーティリティクラスを優先使用
- カスタムCSSが必要な場合はCSS Modulesを使用
- インラインスタイルは避ける
- レスポンシブデザインはTailwindのブレークポイントを使用

### コンポーネント設計原則
- 単一責任の原則に従う
- propsは最小限に抑える
- 再利用可能なコンポーネントを作成
- アクセシビリティを考慮（ARIA属性、キーボードナビゲーション）
- Canvas APIを使用したゲーム描画はパフォーマンスを重視

### アニメーション規約
- Framer Motionを使用した宣言的アニメーション
- パフォーマンスを考慮したアニメーション実装
- ユーザーの動きの設定を尊重（prefers-reduced-motion）

## デプロイメント・品質管理

### インフラ構成
- **Vercel**: ホスティングプラットフォーム
- **GitHub Actions**: CI/CDパイプライン
- **Vercel Analytics**: パフォーマンス監視

### 品質ゲート
- TypeScriptコンパイルエラーなし
- ESLintエラーなし
- テストカバレッジ80%以上
- 全テストパス
- Prettierフォーマット適用済み

### セキュリティ対策
- 依存関係の定期的な更新（npm audit）
- GitHub Dependabotによる自動セキュリティアップデート
- 環境変数による機密情報管理

## 開発環境

### 推奨エディタ
- **Visual Studio Code**: TypeScriptとReactの優れたサポート

### 必須VS Code拡張機能
- TypeScript Hero
- ES7+ React/Redux/React-Native snippets
- Prettier - Code formatter
- ESLint
- Tailwind CSS IntelliSense

### 開発支援ツール
- **Git + GitHub**: バージョン管理とコラボレーション
- **MkDocs**: プロジェクトドキュメント
- **GitHub Projects**: タスク管理

## 禁止事項

### 使用禁止技術
- **Redux Toolkit**: 複雑すぎるため使用禁止
- **Styled Components**: パフォーマンス懸念により禁止
- **CSS-in-JS**: ビルド複雑化のため禁止
- **Vue.js**: エコシステムが小規模のため不採用
- **yarn/pnpm**: npmのみ使用

### コーディング禁止事項
- `any`型の使用
- クラスコンポーネントの使用
- インラインスタイルの多用
- useEffectの依存配列省略
- テストのないコード追加

## パッケージ管理

### 必須依存関係
```json
{
  "dependencies": {
    "react": "^18.2.0",
    "react-dom": "^18.2.0",
    "zustand": "^4.4.0",
    "framer-motion": "^10.16.0",
    "@tensorflow/tfjs": "^4.10.0"
  },
  "devDependencies": {
    "@types/react": "^18.2.0",
    "@types/react-dom": "^18.2.0",
    "typescript": "^5.0.0",
    "vite": "^4.4.0",
    "@vitejs/plugin-react": "^4.0.0",
    "tailwindcss": "^3.3.0",
    "eslint": "^8.45.0",
    "prettier": "^3.0.0",
    "vitest": "^0.34.0",
    "@testing-library/react": "^13.4.0",
    "@playwright/test": "^1.37.0"
  }
}
```

### バージョン管理方針
- メジャーバージョンアップは慎重に検討
- セキュリティアップデートは即座に適用
- 月次で依存関係の更新確認