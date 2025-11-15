# 販売管理データベース (Go)

販売管理システムのデータベース層を Go で実装したプロジェクトです。TDD アプローチとドメイン駆動設計を採用しています。

## 技術スタック

### コア技術
- **Go**: 1.21+
- **データベース**: PostgreSQL 16
- **SQL ライブラリ**: sqlx
- **PostgreSQL ドライバ**: lib/pq
- **テスティング**: testify, testcontainers-go

### 開発ツール
- **マイグレーション**: golang-migrate
- **静的解析**: golangci-lint (13+ linters)
- **フォーマット**: goimports, gofmt
- **タスクランナー**: just
- **ファイル監視**: fswatch
- **環境変数管理**: godotenv

## プロジェクト構造

```
.
├── cmd/
│   ├── seed/             # シードデータ投入プログラム
│   └── server/           # アプリケーションエントリーポイント
├── internal/
│   ├── model/            # ドメインモデル
│   └── repository/       # データアクセス層（リポジトリパターン）
├── pkg/
│   ├── database/         # データベース接続管理
│   └── testutil/         # テストユーティリティ
├── test/                 # テストヘルパー
├── migrations/           # データベースマイグレーション
├── .env                  # 環境変数（gitignore 対象）
├── docker-compose.yml    # Docker 環境定義
├── Justfile              # タスク定義
└── .golangci.yml         # 静的解析設定
```

## 環境構築

### 前提条件

- Go 1.21 以上
- Docker Desktop
- just (タスクランナー)

### 初期セットアップ

1. **環境変数の設定**

```bash
cp .env.example .env
# .env を編集して環境に合わせて設定
```

2. **依存関係のインストール**

```bash
go mod download
```

3. **開発ツールのインストール**

```bash
# golang-migrate
go install -tags 'postgres' github.com/golang-migrate/migrate/v4/cmd/migrate@latest

# golangci-lint
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest

# goimports
go install golang.org/x/tools/cmd/goimports@latest

# just
brew install just
# または
cargo install just

# fswatch (macOS)
brew install fswatch
```

4. **Git フックの設定**

```bash
chmod +x setup-hooks.sh
./setup-hooks.sh
```

5. **Docker 環境の起動**

```bash
docker compose up -d postgres
```

6. **マイグレーションの実行**

```bash
# golang-migrate を使用（推奨）
migrate -path migrations -database "postgres://postgres:password@localhost:5432/sales_management?sslmode=disable" up

# または GOPATH 経由
$(go env GOPATH)/bin/migrate -path migrations -database "${DATABASE_URL}" up
```

### データベース接続

PostgreSQL データベースに接続する方法は複数あります。

#### 方法 1: IDE（IntelliJ IDEA / DataGrip）

**接続設定:**

| パラメータ | 値 |
|----------|-----|
| Host | localhost |
| Port | 5432 |
| Database | sales_management |
| User | postgres |
| Password | password |
| URL | jdbc:postgresql://localhost:5432/sales_management?sslmode=disable |

**手順:**

1. `View` → `Tool Windows` → `Database`
2. `+` → `Data Source` → `PostgreSQL`
3. 上記の接続情報を入力
4. `Download missing driver files` でドライバをインストール
5. `Test Connection` で接続確認
6. `OK` で保存

**重要:** SSL を無効化するため、Advanced タブで `sslmode=disable` を設定してください。

#### 方法 2: Adminer（Web UI）- 推奨

Adminer は Web ベースのデータベース管理ツールです。

```bash
# Adminer を起動
docker compose up -d adminer

# ブラウザでアクセス
open http://localhost:8080
```

**ログイン情報:**
```
System: PostgreSQL
Server: postgres
Username: postgres
Password: password
Database: sales_management
```

#### 方法 3: コマンドライン（psql）

```bash
# Docker 経由で接続
docker exec -it sales-management-postgres psql -U postgres -d sales_management

# よく使うコマンド
\dt                 # テーブル一覧
\d テーブル名       # テーブル構造確認
\l                  # データベース一覧
\q                  # 終了
```

#### 接続確認

```bash
# コンテナ状態確認
docker compose ps

# データベース接続テスト
docker exec sales-management-postgres psql -U postgres -d sales_management -c "SELECT version();"

# テーブル一覧
docker exec sales-management-postgres psql -U postgres -d sales_management -c "\dt"
```

#### トラブルシューティング

**接続できない場合:**

1. **コンテナの確認**
   ```bash
   docker compose ps postgres
   # ステータスが "Up (healthy)" であることを確認
   ```

2. **ポートの確認**
   ```bash
   nc -zv localhost 5432
   # "succeeded" が表示されることを確認
   ```

3. **データベースの存在確認**
   ```bash
   docker exec sales-management-postgres psql -U postgres -l | grep sales_management
   ```

4. **コンテナの再起動**
   ```bash
   docker compose restart postgres
   ```

5. **完全リセット（データが削除されます）**
   ```bash
   docker compose down -v
   docker compose up -d postgres
   # マイグレーションを再実行
   ```

## 開発ワークフロー

### タスクランナーの使用

プロジェクトでは `just` を使用してタスクを管理しています。

```bash
# 利用可能なタスク一覧
just --list
# または単に
just

# よく使うタスク
just test           # テスト実行
just watch-test     # テスト自動実行（ファイル変更監視）
just lint           # 静的解析
just fmt            # コードフォーマット
just build          # ビルド
just run            # アプリケーション実行
just seed           # シードデータを投入
just db-up          # データベース起動
just db-down        # データベース停止
just db-reset       # データベースをリセットしてシードデータを投入
just migrate-up     # マイグレーション実行
just migrate-down   # マイグレーションロールバック
```

### TDD サイクル

このプロジェクトは TDD (Test-Driven Development) アプローチを採用しています。

1. **Red**: 失敗するテストを書く
2. **Green**: テストを通す最小限のコードを実装
3. **Refactor**: コードをリファクタリング

### コード品質チェック

コミット前に自動的に以下がチェックされます (pre-commit hook):

- コードフォーマット (goimports, gofmt)
- 静的解析 (golangci-lint)
- テスト実行

手動でチェックする場合:

```bash
just check
```

## データベース

### データベース接続情報

**詳細な接続手順は「環境構築 → データベース接続」セクションを参照してください。**

**クイックリファレンス:**
```
Host: localhost
Port: 5432
Database: sales_management
User: postgres
Password: password
```

詳細なトラブルシューティングは `test_jdbc_connection.md` を参照してください。

### マイグレーション

```bash
# マイグレーション適用
just migrate-up
# または直接
migrate -path migrations -database "${DATABASE_URL}" up

# マイグレーションロールバック
just migrate-down

# マイグレーション状態確認
just migrate-status

# 新しいマイグレーション作成
just migrate-create <migration_name>
# または直接
migrate create -ext sql -dir migrations -seq <migration_name>
```

**マイグレーションファイル:**
- `000001_init_schema.up.sql`: 初期スキーマ
- `000002_create_department_table.up.sql`: 部門マスタ
- `000003_create_employee_table.up.sql`: 社員マスタ

### シードデータ

B社（食肉製造・販売会社）の業務に適したサンプルデータを投入できます。

```bash
# シードデータの投入
just seed

# データベースをリセットしてシードデータを投入
just db-reset
```

**投入されるデータ:**
- 部門マスタ: 21部門（4階層組織構造）
- 社員マスタ: 41名
- 取引先グループマスタ: 7グループ
- 取引先マスタ: 14社（得意先10社、仕入先4社）
- 顧客マスタ: 10社
- 仕入先マスタ: 4社
- 商品分類マスタ: 5分類（牛肉、豚肉、鶏肉、加工品、その他）
- 商品マスタ: 20商品
- 倉庫マスタ: 2倉庫

**特徴:**
- トランザクション管理による一貫性保証
- 外部キー制約を考慮した投入順序
- TRUNCATE CASCADE による既存データクリア
- 実際のビジネスシーンを想定したリアルなデータ

**カスタマイズ:**

環境変数で接続先を変更できます:

```bash
DATABASE_URL="host=localhost port=5432 user=postgres password=mypass dbname=mydb sslmode=disable" just seed
```

### テスト用データベース

テストでは testcontainers を使用して、各テストごとに独立した PostgreSQL コンテナを起動します。

- イメージ: \`postgres:16-alpine\`
- 自動クリーンアップ: テスト終了後に自動削除
- 完全な分離: テスト間でデータが混在しない

## テスト

### テスト実行

```bash
# すべてのテスト
just test

# 特定のパッケージ
go test -v ./internal/repository/...

# 特定のテスト
go test -v ./internal/repository/... -run TestDepartmentRepository_Create

# カバレッジ付き
just test-coverage

# ファイル変更監視でテスト自動実行
just watch-test
```

### テストスタイル

- **BDD スタイル**: Given-When-Then パターン
- **テーブル駆動**: 複数のテストケースを構造化
- **統合テスト**: 実際の PostgreSQL を使用

### テスト例

```go
t.Run("部門を登録できる", func(t *testing.T) {
    // Given: 部門の情報が与えられた時
    dept := &model.Department{...}

    // When: 部門を登録すると
    err := repo.Create(dept)

    // Then: エラーなく登録できる
    require.NoError(t, err)
})
```

## アーキテクチャ

### レイヤー構成

1. **Model (ドメイン層)**
   - ビジネスロジックとドメインモデル
   - 他の層に依存しない

2. **Repository (データアクセス層)**
   - データベース操作の抽象化
   - CRUD 操作を提供

3. **Database (インフラ層)**
   - データベース接続管理
   - 接続プール設定

### 設計パターン

- **Repository パターン**: データアクセスの抽象化
- **Dependency Injection**: 依存性の注入
- **ドメイン駆動設計**: ビジネスロジックの明確化

### 命名規則

- **フィールド名**: 英語（Go の慣例に従う）
- **DB カラム名**: 日本語（ビジネス要件）
- **マッピング**: db タグで対応

```go
type Employee struct {
    EmployeeCode string \`db:"社員コード"\`
    EmployeeName string \`db:"社員名"\`
}
```

## 実装状況

### ✅ Chapter 0: 環境構築

- [x] プロジェクト初期化
- [x] データベース接続
- [x] golang-migrate セットアップ
- [x] Docker Compose 環境
- [x] 静的解析ツール (golangci-lint, goimports)
- [x] タスクランナー (just)
- [x] Git フック

### ✅ Chapter 1: 部門と従業員

#### 部門マスタ
- [x] ドメインモデル (\`internal/model/department.go\`)
- [x] マイグレーション (\`migrations/000002_*.sql\`)
- [x] リポジトリ (\`internal/repository/department_repository.go\`)
- [x] テスト (4 tests)
  - Create
  - FindAll
  - Update
  - Delete

#### 社員マスタ
- [x] ドメインモデル (\`internal/model/employee.go\`)
- [x] マイグレーション (\`migrations/000003_*.sql\`)
- [x] リポジトリ (\`internal/repository/employee_repository.go\`)
- [x] テスト (5 tests)
  - Create
  - FindByDepartment
  - FindAll
  - Update
  - Delete

### テスト結果

```bash
$ just test
テストを実行中...
ok  	github.com/k2works/sales-management-db/internal/repository	33.142s
✅ 9/9 tests passed
```

## トラブルシューティング

### データベース接続エラー

**IDE から接続できない場合:**

詳細なトラブルシューティングは `test_jdbc_connection.md` を参照してください。

**基本的な確認手順:**

```bash
# 1. Docker コンテナの状態確認
docker compose ps postgres
# ステータスが "Up (healthy)" であることを確認

# 2. ポート確認
nc -zv localhost 5432
# "succeeded" が表示されることを確認

# 3. データベース存在確認
docker exec sales-management-postgres psql -U postgres -l | grep sales_management

# 4. 接続テスト
docker exec sales-management-postgres psql -U postgres -d sales_management -c "SELECT version();"

# 5. ログ確認
docker compose logs postgres

# 6. コンテナ再起動
just db-restart
# または
docker compose restart postgres

# 7. 完全リセット（データが削除されます）
just reset
# または手動で
docker compose down -v
docker compose up -d postgres
# マイグレーションを再実行
just migrate-up
```

**IDE 特有の問題:**

- PostgreSQL ドライバがダウンロード済みか確認
- SSL を無効化: Advanced タブで `sslmode=disable` を設定
- URL: `jdbc:postgresql://localhost:5432/sales_management?sslmode=disable`

**代替アクセス方法:**

Adminer (Web UI) を使用:
```bash
docker compose up -d adminer
open http://localhost:8080
```

### テスト失敗

```bash
# テストコンテナのクリーンアップ
docker system prune -f

# 依存関係の再インストール
go mod tidy
go mod download
```

### マイグレーションエラー

```bash
# マイグレーション状態確認
just migrate-status

# 強制的にバージョンを設定
just migrate-force <version>
# または直接
migrate -path migrations -database "${DATABASE_URL}" force <version>
```

## 参考資料

- [Go プロジェクト構成](https://github.com/golang-standards/project-layout)
- [sqlx ドキュメント](https://jmoiron.github.io/sqlx/)
- [testcontainers-go](https://golang.testcontainers.org/)
- [golang-migrate](https://github.com/golang-migrate/migrate)
- [just - コマンドランナー](https://github.com/casey/just)

## ライセンス

MIT
