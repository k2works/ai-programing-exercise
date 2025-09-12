# 技術スタック選定書 - 会議室予約システム

## 概要

本文書では、会議室予約システムのバックエンド・フロントエンド技術スタックの選定理由と詳細な仕様を記載します。選定は `@docs/design/architecture_backend.md` および `@docs/design/architecture_frontend.md` に基づいて実施されました。

## バックエンド技術スタック

### 基盤技術

| カテゴリ | 技術 | バージョン | 選定理由 |
|----------|------|------------|----------|
| **プログラミング言語** | Java | 21 LTS | Records、Switch式等の最新機能活用、長期サポート |
| **フレームワーク** | Spring Boot | 3.3.2 | ヘキサゴナルアーキテクチャ対応、成熟したエコシステム |
| **セキュリティ** | Spring Security | 6.3.1 | JWT Bearer認証、ロールベースアクセス制御 |
| **データアクセス** | Spring Data JPA | 3.3.2 | ドメインモデルパターンのリポジトリ実装支援 |
| **ユーティリティ** | Apache Commons Lang3 | 3.13.0 | 文字列操作、null安全処理、オブジェクトユーティリティ |

### データベース・永続化

| カテゴリ | 技術 | バージョン | 用途・選定理由 |
|----------|------|------------|----------------|
| **本番データベース** | PostgreSQL | 15 | ACIDトランザクション、JSON型サポート、高可用性 |
| **開発・テスト用DB** | H2 Database | 2.2.224 | インメモリDB、高速テスト実行 |
| **マイグレーション** | Flyway | 9.22.3 | バージョン管理されたデータベーススキーマ管理 |
| **接続プール** | HikariCP | 5.0.1 | 高性能コネクションプール |
| **ページネーション** | Spring Data JPA Pageable | 3.3.2 | 効率的なページング処理、カウントクエリ最適化 |

### ビルド・依存関係管理

| カテゴリ | 技術 | バージョン | 選定理由 |
|----------|------|------------|----------|
| **ビルドツール** | Gradle | 8.14 | 高速ビルド、Kotlin DSL、柔軟な設定、増分ビルド |
| **Java実行環境** | OpenJDK | 21 | オープンソース、企業採用実績 |

### テスト・品質管理

| カテゴリ | 技術 | バージョン | 用途・選定理由 |
|----------|------|------------|----------------|
| **単体テスト** | JUnit 5 | 5.10.3 | 現代的なテストフレームワーク |
| **モッキング** | Mockito | 5.5.0 | 成熟したモックライブラリ |
| **統合テスト** | Testcontainers | 1.19.0 | 実際のデータベースを使った統合テスト |
| **アーキテクチャテスト** | ArchUnit | 1.1.0 | アーキテクチャ制約の自動検証 |
| **アサーション** | AssertJ | 3.24.2 | 流暢なアサーション記法 |
| **BDD テスト** | Cucumber | 7.18.0 | 自然言語による受け入れテスト、ビジネス要件の検証 |

### 品質管理・静的解析

| カテゴリ | 技術 | バージョン | 用途 |
|----------|------|------------|------|
| **静的解析** | SonarQube | Community | コード品質・セキュリティ解析 |
| **コーディング規約** | Checkstyle | 10.12.3 | コーディング規約チェック |
| **バグ検出** | SpotBugs | 6.0.7 (Plugin) | 潜在的バグの静的検出 |
| **複雑度・品質チェック** | PMD | 7.0.0 | Java 21対応、複雑度・コード品質分析 |
| **テストカバレッジ** | JaCoCo | 0.8.12 | テストカバレッジ測定・レポート生成 |
| **依存関係チェック** | OWASP Dependency Check | 8.4.0 | 脆弱性のある依存関係の検出 |

### 監視・運用

| カテゴリ | 技術 | バージョン | 用途 |
|----------|------|------------|------|
| **メトリクス** | Micrometer | 1.11.4 | アプリケーションメトリクス収集 |
| **監視** | Prometheus | (外部) | メトリクス監視 |
| **分散トレーシング** | Jaeger | (外部) | リクエストトレーシング |
| **ログ出力** | Logback | 1.4.11 | 構造化ログ（JSON形式） |

## フロントエンド技術スタック

### 基盤技術

| カテゴリ | 技術 | バージョン | 選定理由 |
|----------|------|------------|----------|
| **プログラミング言語** | TypeScript | 5.4.0 | 型安全性、開発者体験の向上 |
| **ライブラリ** | React | 18.3.0 | 成熟したエコシステム、高いインタラクティビティ |
| **ビルドツール** | Vite | 5.2.0 | 高速ビルド、優れた開発体験 |

### ルーティング・状態管理

| カテゴリ | 技術 | バージョン | 選定理由・用途 |
|----------|------|------------|----------------|
| **ルーティング** | React Router DOM | 6.23.0 | SPAルーティング、宣言的ナビゲーション |
| **クライアント状態管理** | Zustand | 4.5.0 | 軽量、シンプルな状態管理 |
| **サーバー状態管理** | TanStack React Query | 5.28.0 | サーバー状態とキャッシュ管理に特化 |

### UI・スタイリング

| カテゴリ | 技術 | バージョン | 選定理由 |
|----------|------|------------|----------|
| **CSSフレームワーク** | Tailwind CSS | 3.4.0 | ユーティリティファースト、高いカスタマイズ性 |
| **CSS Modules** | (Vite内蔵) | - | スタイルの局所化、パフォーマンス最適化 |
| **クラス名結合** | clsx | 2.1.0 | 条件付きクラス名の効率的な管理 |
| **アイコン** | Lucide React | 0.365.0 | 軽量、一貫したアイコンセット |

### フォーム・バリデーション

| カテゴリ | 技術 | バージョン | 選定理由 |
|----------|------|------------|----------|
| **フォーム管理** | React Hook Form | 7.51.0 | パフォーマンス重視、非制御コンポーネント |
| **スキーマバリデーション** | Zod | 3.22.0 | TypeScript統合、実行時型検証 |
| **バリデーション統合** | @hookform/resolvers | 3.3.0 | React Hook FormとZodの統合 |

### HTTP・API

| カテゴリ | 技術 | バージョン | 用途 |
|----------|------|------------|------|
| **HTTPクライアント** | Axios | 1.6.8 | リクエスト・レスポンス インターセプター |
| **API型生成** | (手動定義) | - | バックエンドAPIとの型同期 |

### 開発・ビルドツール

| カテゴリ | 技術 | バージョン | 用途 |
|----------|------|------------|------|
| **React プラグイン** | @vitejs/plugin-react | 4.2.0 | React JSX/TSXサポート |
| **パッケージマネージャー** | npm | 10.x | 依存関係管理 |

### リンティング・フォーマッティング

| カテゴリ | 技術 | バージョン | 用途 |
|----------|------|------------|------|
| **ESLint** | eslint | 8.57.0 | コード品質・一貫性チェック |
| **TypeScript ESLint** | @typescript-eslint/eslint-plugin | 7.6.0 | TypeScript固有のルール |
| **フォーマッター** | Prettier | 3.2.0 | コードフォーマット統一 |

### テスト・品質管理

| カテゴリ | 技術 | バージョン | 用途 |
|----------|------|------------|------|
| **テストランナー** | Vitest | 1.4.0 | 高速テスト実行、Vite統合 |
| **Reactテスト** | @testing-library/react | 14.2.0 | コンポーネントテスト |
| **アサーション** | @testing-library/jest-dom | 6.4.0 | DOM要素のアサーション |
| **API モック** | MSW | 2.2.0 | API レスポンスのモック |

### パフォーマンス最適化

| カテゴリ | 技術 | バージョン | 用途 |
|----------|------|------------|------|
| **仮想スクロール** | react-window | 1.8.8 | 大量データの効率的描画 |
| **コード分割** | React.lazy | (React内蔵) | 動的インポート |
| **バンドル分析** | rollup-plugin-visualizer | 5.12.0 | バンドルサイズ分析 |

### ページネーション

| カテゴリ | 技術 | バージョン | 用途 |
|----------|------|------------|------|
| **ページネーション** | (カスタム実装) | - | TanStack Query + Zustand による状態管理 |
| **無限スクロール** | react-window | 1.8.8 | 大量データの段階的読み込み |

## インフラストラクチャ・運用

### コンテナ・デプロイメント

| カテゴリ | 技術 | バージョン | 用途 |
|----------|------|------------|------|
| **コンテナ** | Docker | 24.0.x | アプリケーションコンテナ化 |
| **ベースイメージ** | eclipse-temurin | 21-jre | OpenJDK公式イメージ |
| **フロントエンド配信** | nginx | alpine | 静的ファイル配信 |

### CI/CD

| カテゴリ | 技術 | 用途 |
|----------|------|------|
| **CI/CD** | GitHub Actions | 自動テスト・ビルド・デプロイ |
| **アーティファクト管理** | GitHub Container Registry | Dockerイメージレジストリ |

## 開発環境・ツール

### IDE・エディタ

| ツール | 用途 |
|--------|------|
| **IntelliJ IDEA** | Javaバックエンド開発 |
| **VS Code** | TypeScriptフロントエンド開発 |

### バージョン管理

| ツール | 用途 |
|--------|------|
| **Git** | ソースコード管理 |
| **GitHub** | コードホスティング・コラボレーション |

## ページネーション設計

### バックエンド実装方針

| レイヤー | 実装技術 | 詳細 |
|----------|----------|------|
| **リポジトリ** | Spring Data JPA Pageable | `Page<Entity> findBy...(Pageable pageable)` |
| **ユースケース** | PagedResult<T> | ドメインモデル用ページング結果 |
| **Web API** | PagedResponse<T> | RESTful APIレスポンス形式 |
| **パフォーマンス** | カウントクエリ分離 | 大量データ対応の最適化 |

### バックエンド実装例

```java
// リポジトリ層
Page<Reservation> findByUserId(UserId userId, Pageable pageable);

// ユースケース層
PagedResult<ReservationSearchResult> execute(SearchReservationsQuery query);

// Web層
@GetMapping("/reservations")
ResponseEntity<PagedResponse<ReservationResponse>> getReservations(
    @RequestParam(defaultValue = "0") int page,
    @RequestParam(defaultValue = "20") int size,
    @RequestParam(defaultValue = "createdAt") String sortBy
);
```

### フロントエンド実装方針

| レイヤー | 実装技術 | 詳細 |
|----------|----------|------|
| **データ取得** | TanStack React Query | ページネーションクエリの自動管理 |
| **状態管理** | Zustand | ページング状態（page, size, sortBy）管理 |
| **UIコンポーネント** | カスタム実装 | Tailwind CSSによるページネーションUI |
| **パフォーマンス** | react-window | 大量データの仮想スクロール対応 |

### フロントエンド実装例

```typescript
// 型定義
interface PaginationParams {
  page: number;
  size: number;
  sortBy: string;
}

interface PagedResponse<T> {
  content: T[];
  totalElements: number;
  totalPages: number;
  currentPage: number;
  size: number;
  hasNext: boolean;
  hasPrevious: boolean;
}

// React Query フック
const useReservations = (params: PaginationParams) => {
  return useQuery({
    queryKey: ['reservations', params],
    queryFn: () => reservationApi.getReservations(params),
    keepPreviousData: true, // ページ切り替え時のUX向上
  });
};

// Zustand ストア
interface PaginationStore {
  page: number;
  size: number;
  sortBy: string;
  setPage: (page: number) => void;
  setSize: (size: number) => void;
  setSortBy: (sortBy: string) => void;
}

// ページネーションコンポーネント
const Pagination: React.FC<{
  currentPage: number;
  totalPages: number;
  onPageChange: (page: number) => void;
}> = ({ currentPage, totalPages, onPageChange }) => (
  <div className="flex items-center justify-center space-x-2">
    <Button 
      disabled={currentPage <= 1}
      onClick={() => onPageChange(currentPage - 1)}
    >
      前へ
    </Button>
    <span className="px-3 py-1 text-sm">
      {currentPage} / {totalPages}
    </span>
    <Button 
      disabled={currentPage >= totalPages}
      onClick={() => onPageChange(currentPage + 1)}
    >
      次へ
    </Button>
  </div>
);
```

### 無限スクロール実装例

```typescript
// 無限スクロール用フック
const useInfiniteReservations = (filters: ReservationFilters) => {
  return useInfiniteQuery({
    queryKey: ['reservations-infinite', filters],
    queryFn: ({ pageParam = 0 }) =>
      reservationApi.getReservations({ ...filters, page: pageParam }),
    getNextPageParam: (lastPage) => 
      lastPage.hasNext ? lastPage.currentPage + 1 : undefined,
  });
};

// react-window による仮想化リスト
import { FixedSizeList as List } from 'react-window';

const VirtualizedReservationList: React.FC<{
  reservations: Reservation[];
}> = ({ reservations }) => (
  <List
    height={600}
    itemCount={reservations.length}
    itemSize={80}
    itemData={reservations}
  >
    {({ index, style, data }) => (
      <div style={style}>
        <ReservationItem reservation={data[index]} />
      </div>
    )}
  </List>
);
```

### インデックス最適化

```sql
CREATE INDEX idx_reservation_user_created ON reservations(user_id, created_at DESC);
CREATE INDEX idx_reservation_room_datetime ON reservations(room_id, start_time);
```

## セキュリティ対策

### バックエンド

| カテゴリ | 技術・手法 | 詳細 |
|----------|------------|------|
| **認証** | JWT Bearer Token | ステートレス認証 |
| **認可** | Spring Security RBAC | ロールベースアクセス制御 |
| **入力検証** | Bean Validation | @Valid、@Pattern等 |
| **SQLインジェクション** | JPA Prepared Statement | パラメータ化クエリ |

### フロントエンド

| カテゴリ | 技術・手法 | 詳細 |
|----------|------------|------|
| **XSS対策** | DOMPurify | HTMLサニタイズ |
| **CSP** | Content Security Policy | コンテンツセキュリティポリシー |
| **HTTPS強制** | nginx設定 | 暗号化通信 |
| **依存関係監査** | npm audit | 脆弱性チェック |

## 品質メトリクス・目標値

### パフォーマンス

| 項目 | 目標値 | 測定方法 |
|------|--------|----------|
| **API応答時間** | 1秒以内 | Micrometer + Prometheus |
| **フロントエンド初期表示** | 3秒以内 | Lighthouse |
| **バンドルサイズ** | 500KB以下 | Rollup Bundle Analyzer |

### 品質

| 項目 | 目標値 | 測定ツール |
|------|--------|------------|
| **テストカバレッジ** | 80%以上 | JaCoCo (BE) / Vitest (FE) |
| **コード品質** | A評価 | SonarQube |
| **脆弱性** | High/Critical 0件 | OWASP Dependency Check |

### 可用性

| 項目 | 目標値 |
|------|--------|
| **アップタイム** | 99.9%以上 |
| **同時接続ユーザー数** | 100名以上 |

## アップグレード計画

### バックエンド

| 技術 | 現在バージョン | 予定アップグレード | 実施予定 |
|------|----------------|-------------------|----------|
| Java | 21 LTS | 25 LTS | 2025年Q3 |
| Spring Boot | 3.3.x | 3.4.x | 2025年Q1 |
| PostgreSQL | 15 | 16 | 2025年Q2 |

### フロントエンド

| 技術 | 現在バージョン | 予定アップグレード | 実施予定 |
|------|----------------|-------------------|----------|
| React | 18.x | 19.x | 2025年Q2 |
| TypeScript | 5.4.x | 5.5.x | 2025年Q1 |
| Vite | 5.x | 6.x | 2025年Q2 |

## 技術選定の変更管理

### 変更プロセス

1. **技術評価**: 新技術の評価・PoC実施
2. **ADR作成**: Architecture Decision Recordの作成
3. **チーム合意**: 開発チーム内での合意形成
4. **段階的導入**: 小規模機能から段階的に適用
5. **文書更新**: 本技術スタック文書の更新

### 変更基準

- **セキュリティ脆弱性**: Critical/High脆弱性への対応
- **パフォーマンス向上**: 有意な性能改善が期待される場合
- **開発者体験**: 生産性向上が期待される場合
- **長期保守性**: 技術の EOL 対応

### 実装時の主要変更履歴

| 変更日 | 技術 | 変更前 | 変更後 | 理由 |
|--------|------|--------|--------|------|
| 2025-09-12 | Gradle | 8.5 | 8.14 | 最新安定版、ビルドパフォーマンス向上 |
| 2025-09-12 | JUnit 5 | 5.10.0 | 5.10.3 | セキュリティパッチ、安定性向上 |
| 2025-09-12 | PMD | (未記載) | 7.0.0 | Java 21完全対応、複雑度チェック強化 |
| 2025-09-12 | SpotBugs | 4.7.3 | 6.0.7 | プラグインバージョン、最新脆弱性検出 |
| 2025-09-12 | JaCoCo | (未記載) | 0.8.12 | Java 21対応、テストカバレッジ測定 |
| 2025-09-12 | Cucumber | (未記載) | 7.18.0 | BDD受け入れテスト、ビジネス要件の自然言語検証 |
| 2025-09-12 | Apache Commons Lang3 | (未使用) | 3.13.0 | 文字列操作・null安全処理ユーティリティ、開発効率向上 |

この技術スタックにより、保守性、拡張性、セキュリティを確保した会議室予約システムを構築できます。