# 実践データベース設計（C# / .NET 版）

Dapper + FluentMigrator + Testcontainers を使用したテスト駆動データベース設計の実践プロジェクトです。

## プロジェクト概要

このプロジェクトは、テスト駆動開発（TDD）の手法を用いてデータベース設計を行う実践的なサンプルです。販売管理システムを題材に、部門・社員管理から始まり、段階的にデータベース設計を学習します。

### 技術スタック

- **.NET 9.0** - アプリケーションフレームワーク
- **ASP.NET Core** - Web API フレームワーク
- **Dapper** - 軽量マイクロ ORM
- **FluentMigrator** - データベースマイグレーションツール
- **Swashbuckle (Swagger)** - API ドキュメント生成
- **Testcontainers** - Docker ベースの統合テスト環境
- **xUnit** - テスティングフレームワーク
- **FluentAssertions** - アサーションライブラリ
- **Cake** - ビルド自動化ツール
- **PostgreSQL** - データベース（開発環境）

### プロジェクト構成

```
SalesManagement.sln
├── SalesManagement.Domain/          # ドメイン層
│   └── Models/                      # エンティティクラス
│       ├── Department.cs            # 部門マスタ
│       ├── Employee.cs              # 社員マスタ
│       ├── Product.cs               # 商品マスタ
│       └── ...                      # その他のエンティティ
├── SalesManagement.Infrastructure/  # インフラストラクチャ層
│   ├── Migrations/                  # データベースマイグレーション
│   │   ├── Migration_20250106_001_InitialSetup.cs
│   │   ├── Migration_20250106_002_CreateDepartmentTable.cs
│   │   ├── Migration_20250106_003_CreateEmployeeTable.cs
│   │   └── ...                      # その他のマイグレーション
│   ├── Repositories/                # リポジトリクラス
│   │   ├── DepartmentRepository.cs  # 部門リポジトリ
│   │   ├── EmployeeRepository.cs    # 社員リポジトリ
│   │   ├── ProductRepository.cs     # 商品リポジトリ
│   │   └── ...                      # その他のリポジトリ
│   ├── MigrationRunner.cs           # マイグレーション実行
│   ├── DataSeeder.cs                # テストデータ投入
│   └── Program.cs                   # エントリーポイント
├── SalesManagement.Api/             # API 層（Chapter 9）
│   ├── Controllers/                 # API コントローラ
│   │   └── ProductController.cs     # 商品 API
│   ├── Services/                    # ビジネスロジック
│   │   └── ProductService.cs        # 商品サービス
│   ├── Dtos/                        # データ転送オブジェクト
│   │   ├── CreateProductRequest.cs  # 商品作成リクエスト
│   │   ├── UpdateProductRequest.cs  # 商品更新リクエスト
│   │   ├── ProductResponse.cs       # 商品レスポンス
│   │   └── PageResponse.cs          # ページングレスポンス
│   ├── Exceptions/                  # カスタム例外
│   │   ├── ResourceNotFoundException.cs
│   │   └── BusinessException.cs
│   ├── Middleware/                  # ミドルウェア
│   │   └── GlobalExceptionHandler.cs # グローバル例外ハンドラ
│   └── Program.cs                   # API エントリーポイント
├── SalesManagement.ConsoleApp/      # コンソールアプリ
│   ├── Program.cs                   # マイグレーション＆Seed実行
│   └── appsettings.json             # 設定ファイル
└── SalesManagement.Tests/           # テスト層
    ├── DatabaseTestBase.cs          # テストベースクラス
    ├── Dtos/                        # DTO テスト
    │   └── ProductDtoTests.cs       # 商品 DTO バリデーションテスト
    ├── Controllers/                 # コントローラテスト
    │   └── ProductControllerTests.cs # 商品 API 統合テスト
    └── IntegrationTests/            # 統合テスト
        ├── DatabaseConnectionTests.cs
        ├── DepartmentTests.cs       # 部門マスタテスト
        ├── EmployeeTests.cs         # 社員マスタテスト
        ├── ProductTests.cs          # 商品マスタテスト
        └── ...                      # その他の統合テスト
```

## 環境構築

### 前提条件

- [.NET 9.0 SDK](https://dotnet.microsoft.com/download/dotnet/9.0)
- [Docker Desktop](https://www.docker.com/products/docker-desktop)
- Git

### セットアップ手順

#### 1. リポジトリのクローン

```bash
git clone <repository-url>
cd ai-programing-exercise/db/csharp
```

#### 2. 依存パッケージの復元

```bash
dotnet restore
```

#### 3. Cake のインストール

```bash
dotnet tool restore
```

#### 4. 完全セットアップ（推奨）

Docker 起動、ビルド、マイグレーション、テストを一括実行：

```bash
dotnet cake --target=Setup
```

または個別に実行：

```bash
# Docker コンテナ起動
dotnet cake --target=Docker-Up

# ビルド
dotnet cake --target=Build

# マイグレーション実行
dotnet cake --target=Migrate

# テスト実行
dotnet cake --target=Test
```

## Cake タスク一覧

| タスク | 説明 |
|--------|------|
| `Clean` | ビルド成果物のクリーンアップ |
| `Restore` | NuGet パッケージの復元 |
| `Format` | コードフォーマット |
| `Build` | プロジェクトのビルド |
| `Test` | テスト実行 |
| `Test-Coverage` | カバレッジ測定付きテスト実行 |
| `Coverage-Report` | カバレッジレポート生成 |
| `Watch` | ファイル変更を監視してテストを自動実行 |
| `CI` | CI 環境用の完全チェック |
| `Migrate` | データベースマイグレーション実行 |
| `Docker-Up` | Docker コンテナ起動 |
| `Docker-Down` | Docker コンテナ停止 |
| `Setup` | 完全セットアップ（Docker + ビルド + マイグレーション + テスト） |
| `Reset` | 環境リセット（Docker 再起動 + マイグレーション） |
| `Default` | デフォルトタスク（ビルド + テスト） |

### タスク実行例

```bash
# デフォルトタスク（ビルド + テスト）
dotnet cake

# コードフォーマット
dotnet cake --target=Format

# カバレッジレポート生成
dotnet cake --target=Coverage-Report

# 環境リセット
dotnet cake --target=Reset
```

## 開発の流れ（TDD サイクル）

このプロジェクトは、テスト駆動開発（TDD）の Red-Green-Refactor サイクルに従います。

### 1. Red（失敗するテストを書く）

```csharp
[Fact]
public async Task 部門を登録できる()
{
    // Arrange
    var repository = new DepartmentRepository(ConnectionString);
    var department = new Department { /* ... */ };

    // Act
    await repository.InsertAsync(department);

    // Assert
    var found = await repository.FindByIdAsync("11101", new DateTime(2021, 1, 1));
    found.Should().NotBeNull();
}
```

### 2. Green（テストを通す最小限の実装）

- Entity クラスの作成
- Repository クラスの作成
- マイグレーションの作成

### 3. Refactor（リファクタリング）

- コードの重複排除
- 可読性の向上
- パフォーマンスの最適化

## データベース設計

### 第 1 章：部門と従業員

#### 部門マスタ（Department）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 部門コード | DepartmentCode | VARCHAR(20) | ○ | 部門の一意識別子 |
| 開始日 | StartDate | DATE | ○ | 部門の有効開始日（履歴管理） |
| 終了日 | EndDate | DATE | | 部門の有効終了日 |
| 部門名 | DepartmentName | VARCHAR(100) | | 部門の名称 |
| 組織階層 | OrganizationLevel | INTEGER | | 組織内での階層レベル |
| 部門パス | DepartmentPath | VARCHAR(500) | | 階層パス（例: 10000/11000/11101） |
| 最下層区分 | LowestLevelFlag | SMALLINT | | 最下層かどうか（0:中間階層, 1:最下層） |
| 伝票入力可否 | SlipInputFlag | SMALLINT | | 伝票入力が可能か（0:不可, 1:可） |

**設計のポイント：**
- 複合主キー（部門コード + 開始日）による履歴管理
- 組織階層と部門パスによる階層構造の表現
- 最下層区分による階層レベルの識別

#### 社員マスタ（Employee）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 社員コード | EmployeeCode | VARCHAR(20) | ○ | 社員の一意識別子 |
| 社員名 | EmployeeName | VARCHAR(100) | | 社員の氏名 |
| 社員名カナ | EmployeeNameKana | VARCHAR(100) | | 社員の氏名（カナ） |
| 性別 | Gender | VARCHAR(1) | | 性別（M:男性, F:女性, O:その他） |
| 生年月日 | BirthDate | DATE | | 生年月日 |
| 入社年月日 | JoinDate | DATE | | 入社年月日 |
| 部門コード | DepartmentCode | VARCHAR(20) | | 所属部門コード |
| 役職コード | PositionCode | VARCHAR(20) | | 役職コード |

**設計のポイント：**
- 部門コードによる部門マスタとの関連
- カナ名のインデックスによるあいまい検索対応
- CHECK 制約による性別の値検証

### 第 2 章：商品の管理

#### 商品分類マスタ（ProductCategory）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 商品分類コード | ProductCategoryCode | VARCHAR(20) | ○ | 商品分類の一意識別子 |
| 商品分類名 | ProductCategoryName | VARCHAR(100) | | 商品分類の名称 |
| 商品分類パス | ProductCategoryPath | VARCHAR(500) | | 階層パス |
| 組織階層 | OrganizationLevel | INTEGER | | 階層レベル |
| 最下層区分 | LowestLevelFlag | SMALLINT | | 最下層かどうか |

**設計のポイント：**
- 階層構造による商品分類の管理
- 部門マスタと同様のパス表現による階層管理

#### 商品マスタ（Product）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 商品コード | ProductCode | VARCHAR(16) | ○ | 商品の一意識別子 |
| 商品正式名 | ProductFormalName | VARCHAR(40) | | 商品の正式名称 |
| 商品略称 | ProductAbbreviation | VARCHAR(10) | | 商品の略称 |
| 商品名カナ | ProductNameKana | VARCHAR(20) | | 商品名（カナ） |
| 商品区分 | ProductType | VARCHAR(10) | | 商品区分（PRODUCT/SERVICE） |
| 販売単価 | SellingPrice | INTEGER | | 標準販売単価 |
| 仕入単価 | PurchasePrice | INTEGER | | 標準仕入単価 |
| 売上原価 | CostOfSales | INTEGER | | 売上原価 |
| 商品分類コード | ProductCategoryCode | VARCHAR(20) | | 商品分類 |
| 仕入先コード | SupplierCode | VARCHAR(8) | | 主要仕入先 |
| 在庫管理対象区分 | InventoryManagementFlag | SMALLINT | | 在庫管理対象か |

**設計のポイント：**
- 商品区分による商品とサービスの区別
- 在庫管理対象区分による在庫管理の有無
- 得意先別単価マスタによる顧客別価格設定

### 第 3 章：顧客と取引先

#### 取引先マスタ（Company）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 取引先コード | CompanyCode | VARCHAR(8) | ○ | 取引先の一意識別子 |
| 取引先名 | CompanyName | VARCHAR(60) | | 取引先の名称 |
| 仕入先区分 | SupplierFlag | SMALLINT | | 仕入先かどうか（0:得意先のみ、1:仕入先） |
| 取引先グループコード | CompanyGroupCode | VARCHAR(4) | | 取引先グループ |
| 郵便番号 | PostalCode | VARCHAR(8) | | 郵便番号 |
| 都道府県 | Prefecture | VARCHAR(10) | | 都道府県 |
| 住所１ | Address1 | VARCHAR(60) | | 住所（市区町村以降） |
| 与信限度額 | CreditLimit | INTEGER | | 与信限度額 |

**設計のポイント：**
- 仕入先区分により得意先と仕入先を統合管理
- 顧客マスタと仕入先マスタで取引先マスタを参照
- 与信限度額による売掛金管理

#### 顧客マスタ（Customer）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 顧客コード | CustomerCode | VARCHAR(8) | ○ | 顧客の一意識別子 |
| 顧客枝番 | CustomerBranch | INTEGER | ○ | 枝番（複数拠点対応） |
| 顧客名 | CustomerName | VARCHAR(60) | | 顧客名 |
| 取引先コード | CompanyCode | VARCHAR(8) | | 親取引先コード |
| 請求先コード | BillingCustomerCode | VARCHAR(8) | | 請求先コード |
| 回収先コード | CollectionCustomerCode | VARCHAR(8) | | 回収先コード |
| 自社担当者コード | OurEmployeeCode | VARCHAR(8) | | 自社担当者 |

**設計のポイント：**
- 複合主キー（顧客コード + 顧客枝番）による拠点管理
- 請求先・回収先の別管理（グループ一括請求対応）
- 自社担当者による営業管理

### 第 4 章：受注と売上

#### 受注ヘッダ（Order）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 受注番号 | OrderNumber | VARCHAR(13) | ○ | 受注の一意識別子 |
| 受注日 | OrderDate | DATE | | 受注日 |
| 顧客コード | CustomerCode | VARCHAR(8) | | 受注先顧客コード |
| 顧客枝番 | CustomerBranch | INTEGER | | 受注先顧客枝番 |
| 納品予定日 | DeliveryDate | DATE | | 納品予定日 |
| 受注金額 | OrderAmount | INTEGER | | 受注金額合計 |
| 受注ステータス | OrderStatus | VARCHAR(10) | | ステータス（ORDERED/SHIPPED/CANCELLED） |

**設計のポイント：**
- ヘッダ・明細パターンによる伝票管理
- 受注ステータスによる進捗管理
- 売上ヘッダとの連携（受注→出荷→売上計上）

#### 売上ヘッダ（Sales）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 売上番号 | SalesNumber | VARCHAR(13) | ○ | 売上の一意識別子 |
| 売上日 | SalesDate | DATE | | 売上計上日 |
| 受注番号 | OrderNumber | VARCHAR(13) | | 元受注番号 |
| 顧客コード | CustomerCode | VARCHAR(8) | | 売上先顧客コード |
| 売上金額 | SalesAmount | INTEGER | | 売上金額合計 |
| 売上原価 | CostOfSales | INTEGER | | 売上原価合計 |

**設計のポイント：**
- 受注番号による受注との紐付け
- 売上明細での商品別売上管理
- 売上原価の自動計算

### 第 5 章：発注と仕入

#### 発注ヘッダ（PurchaseOrder）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 発注番号 | PurchaseOrderNumber | VARCHAR(13) | ○ | 発注の一意識別子 |
| 発注日 | PurchaseOrderDate | DATE | | 発注日 |
| 仕入先コード | SupplierCode | VARCHAR(8) | | 発注先仕入先コード |
| 納品予定日 | DeliveryDate | DATE | | 納品予定日 |
| 発注金額 | PurchaseOrderAmount | INTEGER | | 発注金額合計 |
| 発注ステータス | PurchaseOrderStatus | VARCHAR(10) | | ステータス |

**設計のポイント：**
- 受注と同様のヘッダ・明細パターン
- 仕入ヘッダとの連携（発注→入荷→仕入計上）

#### 仕入ヘッダ（Purchase）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 仕入番号 | PurchaseNumber | VARCHAR(13) | ○ | 仕入の一意識別子 |
| 仕入日 | PurchaseDate | DATE | | 仕入計上日 |
| 発注番号 | PurchaseOrderNumber | VARCHAR(13) | | 元発注番号 |
| 仕入先コード | SupplierCode | VARCHAR(8) | | 仕入先コード |
| 仕入金額 | PurchaseAmount | INTEGER | | 仕入金額合計 |

**設計のポイント：**
- 発注番号による発注との紐付け
- 在庫マスタへの自動反映

### 第 6 章：在庫管理

#### 在庫マスタ（Stock）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 商品コード | ProductCode | VARCHAR(16) | ○ | 商品コード |
| 倉庫コード | WarehouseCode | VARCHAR(4) | ○ | 倉庫コード |
| 在庫数量 | StockQuantity | INTEGER | | 現在庫数量 |
| 引当数量 | AllocatedQuantity | INTEGER | | 引当済み数量 |
| 有効在庫数量 | AvailableQuantity | INTEGER | | 有効在庫（在庫 - 引当） |
| 発注残数量 | OrderedQuantity | INTEGER | | 発注残数量 |

**設計のポイント：**
- 複合主キー（商品コード + 倉庫コード）による倉庫別在庫管理
- 引当数量による在庫引当機能
- 有効在庫数量の自動計算（在庫数量 - 引当数量）
- 発注残数量による入荷予定管理

### 第 7 章：売掛金管理と入金消込

#### 請求ヘッダ（Invoice）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 請求番号 | InvoiceNumber | VARCHAR(13) | ○ | 請求の一意識別子 |
| 請求年月 | InvoiceYearMonth | VARCHAR(6) | | 請求年月（YYYYMM） |
| 顧客コード | CustomerCode | VARCHAR(8) | | 請求先顧客コード |
| 前月繰越額 | PreviousBalance | INTEGER | | 前月繰越額 |
| 当月売上額 | CurrentSalesAmount | INTEGER | | 当月売上額 |
| 当月入金額 | CurrentPaymentAmount | INTEGER | | 当月入金額 |
| 当月請求額 | CurrentInvoiceAmount | INTEGER | | 当月請求額 |
| 消費税額 | TaxAmount | INTEGER | | 消費税額 |

**設計のポイント：**
- 請求年月による月次請求管理
- 前月繰越、当月売上、当月入金の管理
- 請求明細での売上伝票別管理

#### 売掛金マスタ（Credit）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 売上番号 | SalesNumber | VARCHAR(13) | ○ | 売上番号 |
| 顧客コード | CustomerCode | VARCHAR(8) | | 顧客コード |
| 売上日 | SalesDate | DATE | | 売上日 |
| 売上金額 | SalesAmount | INTEGER | | 売上金額 |
| 入金済額 | PaidAmount | INTEGER | | 入金済額 |
| 未入金額 | UnpaidAmount | INTEGER | | 未入金額 |
| 消込状態 | ClearingStatus | VARCHAR(10) | | 消込状態（UNPAID/PARTIAL/CLEARED） |

**設計のポイント：**
- 売上番号を主キーとした売上単位の債権管理
- 入金消込による未入金額の管理
- 消込状態による入金ステータス管理

#### 入金ヘッダ（Payment）

| 物理名 | 論理名 | 型 | PK | 説明 |
|--------|--------|----|----|------|
| 入金番号 | PaymentNumber | VARCHAR(13) | ○ | 入金の一意識別子 |
| 入金日 | PaymentDate | DATE | | 入金日 |
| 顧客コード | CustomerCode | VARCHAR(8) | | 入金元顧客コード |
| 入金金額 | PaymentAmount | INTEGER | | 入金金額 |
| 入金方法 | PaymentMethod | VARCHAR(10) | | 入金方法（CASH/TRANSFER/CHECK） |
| 銀行口座コード | BankAccountCode | VARCHAR(8) | | 入金先銀行口座 |

**設計のポイント：**
- 入金伝票の管理
- 売掛金マスタへの消込処理
- 銀行口座マスタによる入金先管理

### 第 8 章：テストデータ投入

DataSeeder クラスにより、以下のテストデータを自動投入：

- 部門マスタ: 21 部門（階層構造）
- 社員マスタ: 45 名（正社員 24 名 + パート 21 名）
- 取引先グループマスタ: 7 グループ
- 取引先マスタ: 14 社（得意先・仕入先）
- 顧客マスタ: 10 社
- 仕入先マスタ: 4 社
- 商品分類マスタ: 3 分類
- 商品マスタ: 20 商品
- 倉庫マスタ: 2 倉庫

**設計のポイント：**
- ビジネスドメインに即したリアルなデータ
- 食品加工会社（B 社）の業務フローを想定
- トランザクション管理による一貫性保証

## マイグレーション

### マイグレーションの作成

新しいマイグレーションファイルを作成する場合：

```bash
# 例：商品マスタテーブルの作成
# SalesManagement.Infrastructure/Migrations/Migration_YYYYMMDD_NNN_Description.cs
```

マイグレーションクラスの命名規則：
- `Migration{YYYYMMDD}{連番3桁}{説明}` 形式
- 例：`Migration20250106002CreateDepartmentTable`

### マイグレーションの実行

```bash
# Cake タスク経由（推奨）
dotnet cake --target=Migrate

# 直接実行
cd SalesManagement.Infrastructure
dotnet run
```

### マイグレーションのロールバック

FluentMigrator の Down メソッドでロールバック処理を定義：

```csharp
public override void Down()
{
    Delete.Table("部門マスタ");
}
```

## テスト

### テストの実行

```bash
# 全テスト実行
dotnet cake --target=Test

# カバレッジ付きテスト実行
dotnet cake --target=Test-Coverage

# カバレッジレポート生成
dotnet cake --target=Coverage-Report

# ファイル変更を監視して自動テスト
dotnet cake --target=Watch
```

### テストの特徴

- **Testcontainers** による独立したテスト環境
- 各テストクラスごとに独立した PostgreSQL コンテナを起動
- テスト終了後に自動的にコンテナを削除
- マイグレーションを自動実行してスキーマを作成

### テストの構成

```csharp
public class DepartmentTests : DatabaseTestBase
{
    [Fact]
    public async Task 部門を登録できる()
    {
        // Arrange - テストデータの準備
        var repository = new DepartmentRepository(ConnectionString);
        var department = new Department { /* ... */ };

        // Act - テスト対象の実行
        await repository.InsertAsync(department);

        // Assert - 結果の検証
        var found = await repository.FindByIdAsync("11101", new DateTime(2021, 1, 1));
        found.Should().NotBeNull();
        found!.DepartmentName.Should().Be("新規部署");
    }
}
```

## コード品質

### コード解析

プロジェクトには以下のコード解析設定が含まれています：

- `.editorconfig` - エディタ設定とコーディングスタイル
- `.globalconfig` - コードアナライザー設定
  - サイクロマティック複雑度: 7 以下
  - 各種コード品質ルール

### フォーマット

```bash
# コードフォーマット実行
dotnet cake --target=Format

# フォーマットチェックのみ
dotnet format --verify-no-changes
```

## CI/CD

### CI タスク

```bash
dotnet cake --target=CI
```

CI タスクは以下を実行します：
1. クリーンアップ
2. コードフォーマットチェック
3. ビルド
4. カバレッジ付きテスト実行
5. カバレッジレポート生成

## トラブルシューティング

### Docker コンテナが起動しない

```bash
# コンテナの状態確認
docker ps -a

# コンテナを停止して再起動
dotnet cake --target=Reset
```

### マイグレーションが失敗する

```bash
# Docker ログを確認
docker-compose logs postgres

# データベースをリセット
dotnet cake --target=Docker-Down
dotnet cake --target=Docker-Up
dotnet cake --target=Migrate
```

### テストが失敗する

```bash
# 詳細なログ付きでテスト実行
dotnet test --logger "console;verbosity=detailed"

# 特定のテストのみ実行
dotnet test --filter "FullyQualifiedName~DepartmentTests"
```

## 参考資料

- [Dapper Documentation](https://github.com/DapperLib/Dapper)
- [FluentMigrator Documentation](https://fluentmigrator.github.io/)
- [Testcontainers for .NET](https://dotnet.testcontainers.org/)
- [xUnit Documentation](https://xunit.net/)
- [Cake Build Documentation](https://cakebuild.net/)

## ライセンス

このプロジェクトは学習目的のサンプルコードです。

## REST API の使用方法（Chapter 9）

### API サーバーの起動

```bash
cd SalesManagement.Api
dotnet run
```

起動後、以下の URL にアクセス：
- **Swagger UI**: `http://localhost:5218/swagger`
- **API エンドポイント**: `http://localhost:5218/api/products`

### API エンドポイント一覧

| メソッド | エンドポイント | 説明 |
|---------|--------------|------|
| GET | `/api/products` | 全商品取得 |
| GET | `/api/products/page?page=0&size=20` | ページング対応取得 |
| GET | `/api/products/{productCode}` | 商品詳細取得 |
| POST | `/api/products` | 商品作成 |
| PUT | `/api/products/{productCode}` | 商品更新 |
| DELETE | `/api/products/{productCode}` | 商品削除 |

### API 使用例

#### 商品作成

```bash
curl -X POST http://localhost:5218/api/products \
  -H "Content-Type: application/json" \
  -d '{
    "productCode": "PROD0001",
    "fullName": "黒毛和牛サーロインステーキ 200g",
    "name": "サーロイン",
    "kanaName": "クロゲワギュウサーロイン",
    "unitPrice": 5000,
    "primeCost": 3500,
    "supplierCode": "S0000001"
  }'
```

#### 商品一覧取得

```bash
curl http://localhost:5218/api/products
```

#### 商品更新

```bash
curl -X PUT http://localhost:5218/api/products/PROD0001 \
  -H "Content-Type: application/json" \
  -d '{
    "fullName": "黒毛和牛サーロインステーキ 250g",
    "unitPrice": 5500
  }'
```

### アーキテクチャの特徴

- **レイヤードアーキテクチャ**: Controller/Service/Repository の 3 層分離
- **Data Annotations**: 属性ベースの入力検証
- **グローバル例外ハンドリング**: ミドルウェアによる一元的なエラー処理
- **Swagger UI**: 対話的な API ドキュメント
- **TDD アプローチ**: ProductDtoTests、ProductControllerTests による品質保証

## 進捗状況

- [x] 第 0 章：環境構築
- [x] 第 1 章：最初の要求「部門と従業員」
- [x] 第 2 章：商品の管理
- [x] 第 3 章：顧客と取引先
- [x] 第 4 章：受注と売上
- [x] 第 5 章：発注と仕入
- [x] 第 6 章：在庫管理
- [x] 第 7 章：売掛金管理と入金消込
- [x] 第 8 章：テストデータの投入
- [x] 第 9 章：REST API サービスの追加
