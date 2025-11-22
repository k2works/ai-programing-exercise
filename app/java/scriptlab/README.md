# 財務会計システム API クライアント for Excel Script Lab

Excel Script Lab で使用できる財務会計システム REST API の JavaScript クライアントです。

## ファイル構成

- `openapi.yaml` - OpenAPI 3.0 仕様書
- `api-client.js` - JavaScript API クライアント（Script Lab用）
- `README.md` - このドキュメント

## セットアップ

### 1. Spring Boot アプリケーションの起動

```bash
cd app/java
./gradlew bootRun
```

アプリケーションは `http://localhost:8080` で起動します。

### 2. Excel Script Lab でのセットアップ

1. Excel を開く
2. `挿入` タブ → `アドイン` → `Script Lab` を選択
3. Script Lab で新しいスクリプトを作成
4. `api-client.js` の内容をコピーして貼り付け

## 使用方法

### 基本的な使い方

```javascript
// API クライアントを初期化（api-client.js を読み込むと自動的に window.FinancialAccountingAPI が利用可能になります）

// 勘定科目一覧を取得
const accounts = await FinancialAccountingAPI.Accounts.getAllAccounts();
console.log(accounts);

// 特定の勘定科目を取得
const account = await FinancialAccountingAPI.Accounts.getAccount('11');
console.log(account);
```

### 財務分析 API の使用例

```javascript
async function run() {
  await Excel.run(async (context) => {
    const sheet = context.workbook.worksheets.getActiveWorksheet();

    try {
      // 2021年度の財務分析を取得
      const result = await FinancialAccountingAPI.FinancialAnalysis.analyzeByFiscalYear(2021);

      // Excel シートに書き込み
      FinancialAccountingAPI.Excel.writeAnalysisResultToSheet(sheet, result, "A1");

      await context.sync();
      console.log("財務分析結果を書き込みました");
    } catch (error) {
      console.error("エラーが発生しました:", error);
    }
  });
}
```

### 貸借対照表の取得と表示

```javascript
async function run() {
  await Excel.run(async (context) => {
    const sheet = context.workbook.worksheets.getActiveWorksheet();

    try {
      // 2025年3月31日時点の貸借対照表を取得
      const balanceSheet = await FinancialAccountingAPI.FinancialStatements.getBalanceSheet('2025-03-31');

      // Excel シートに書き込み
      FinancialAccountingAPI.Excel.writeBalanceSheetToSheet(sheet, balanceSheet, "A1");

      await context.sync();
      console.log("貸借対照表を書き込みました");
    } catch (error) {
      console.error("エラーが発生しました:", error);
    }
  });
}
```

### 複数期間の比較分析

```javascript
async function run() {
  await Excel.run(async (context) => {
    const sheet = context.workbook.worksheets.getActiveWorksheet();

    try {
      // 2021年度と2022年度を比較分析
      const comparison = await FinancialAccountingAPI.FinancialAnalysis.compareMultiplePeriods([2021, 2022]);

      console.log("比較分析結果:", comparison);
      console.log("トレンド:", comparison.trends);

      // 各期間のデータをシートに書き込み
      let startRow = 1;
      for (const period of comparison.periods) {
        const startCell = `A${startRow}`;
        FinancialAccountingAPI.Excel.writeAnalysisResultToSheet(sheet, period, startCell);
        startRow += 20; // 次の期間は20行下に配置
      }

      await context.sync();
      console.log("比較分析結果を書き込みました");
    } catch (error) {
      console.error("エラーが発生しました:", error);
    }
  });
}
```

### 仕訳の作成

```javascript
async function createJournal() {
  const journalData = {
    journalNo: "J20250101",
    journalDate: "2025-01-01",
    inputDate: "2025-01-01",
    settlementFlag: false,
    singleEntryFlag: false,
    journalType: 1,
    recurringFlag: false,
    employeeCode: "E001",
    departmentCode: "D001",
    redSlipFlag: false,
    entries: [
      {
        lineNumber: 1,
        description: "売上計上",
        lines: [
          {
            debitCreditFlag: "借",
            accountCode: "11",
            amount: 100000,
            baseAmount: 100000,
            currencyCode: "JPY",
            exchangeRate: 1.0
          },
          {
            debitCreditFlag: "貸",
            accountCode: "41",
            amount: 100000,
            baseAmount: 100000,
            currencyCode: "JPY",
            exchangeRate: 1.0
          }
        ]
      }
    ]
  };

  try {
    const result = await FinancialAccountingAPI.Journals.createJournal(journalData);
    console.log("仕訳を作成しました:", result);
    return result;
  } catch (error) {
    console.error("仕訳作成エラー:", error);
    throw error;
  }
}
```

## API リファレンス

### 勘定科目マスタ管理 API (`FinancialAccountingAPI.Accounts`)

| メソッド | 説明 |
|---------|------|
| `getAllAccounts()` | 勘定科目一覧取得 |
| `getAccount(accountCode)` | 勘定科目取得 |
| `createAccount(accountData)` | 勘定科目作成 |
| `updateAccount(accountCode, accountData)` | 勘定科目更新 |
| `deleteAccount(accountCode)` | 勘定科目削除 |

### 仕訳管理 API (`FinancialAccountingAPI.Journals`)

| メソッド | 説明 |
|---------|------|
| `getAllJournals()` | 仕訳一覧取得 |
| `getJournal(journalNo)` | 仕訳取得 |
| `createJournal(journalData)` | 仕訳作成 |
| `updateJournal(journalNo, journalData)` | 仕訳更新 |
| `deleteJournal(journalNo)` | 仕訳削除 |

### イベントソーシング版仕訳 API (`FinancialAccountingAPI.JournalEntriesES`)

| メソッド | 説明 |
|---------|------|
| `createJournalEntry(entryData)` | 仕訳作成（イベントソーシング） |
| `getJournalEntry(id)` | 仕訳取得（イベント再生） |
| `approveJournalEntry(id, approvalData)` | 仕訳承認 |
| `deleteJournalEntry(id, deleteData)` | 仕訳削除 |

### 財務諸表 API (`FinancialAccountingAPI.FinancialStatements`)

| メソッド | 説明 |
|---------|------|
| `getBalanceSheet(asOfDate)` | 貸借対照表取得 |
| `getIncomeStatement(fromDate, toDate)` | 損益計算書取得 |
| `getFinancialRatios(asOfDate, fromDate, toDate)` | 財務指標取得 |

### 財務分析 API (`FinancialAccountingAPI.FinancialAnalysis`)

| メソッド | 説明 |
|---------|------|
| `analyzeByFiscalYear(fiscalYear)` | 会計年度別財務分析 |
| `compareMultiplePeriods(fiscalYears)` | 複数期間比較分析 |

### 監査ログ API (`FinancialAccountingAPI.AuditLogs`)

| メソッド | 説明 |
|---------|------|
| `getAuditLogsByPeriod(startDate, endDate, limit)` | 期間別監査ログ取得 |
| `getUserActivity(userId, startDate, endDate)` | ユーザー操作履歴取得 |
| `getEntityHistory(entityType, entityId)` | エンティティ変更履歴取得 |

### Excel 統合ヘルパー (`FinancialAccountingAPI.Excel`)

| メソッド | 説明 |
|---------|------|
| `getAccountsFromTable(worksheet, tableName)` | Excel テーブルから勘定科目データを取得 |
| `writeAnalysisResultToSheet(worksheet, analysisResult, startCell)` | 財務分析結果を Excel に書き込み |
| `writeBalanceSheetToSheet(worksheet, balanceSheet, startCell)` | 貸借対照表を Excel に書き込み |

## データモデル

### AccountRequest / AccountResponse

```javascript
{
  accountCode: string,      // 勘定科目コード（最大10文字）
  accountName: string,      // 勘定科目名（最大40文字）
  accountAbbr: string,      // 勘定科目略称（最大20文字）
  accountKana: string,      // 勘定科目カナ（最大40文字）
  bsplType: string,         // BS/PL区分（"B" or "P"）
  debitCreditType: string,  // 借方/貸方区分（"借" or "貸"）
  elementType: string,      // 要素区分（最大10文字）
  displayOrder: number      // 表示順序
}
```

### FinancialAnalysisResult

```javascript
{
  fiscalYear: number,       // 会計年度
  financialData: {
    sales: number,                    // 売上高
    operatingProfit: number,          // 営業利益
    totalAssets: number,              // 総資産
    tangibleFixedAssets: number,      // 有形固定資産
    currentAssets: number,            // 流動資産
    currentLiabilities: number,       // 流動負債
    quickAssets: number,              // 当座資産
    equity: number                    // 純資産
  },
  ratios: {
    profitability: {
      operatingProfitMargin: number   // 売上高営業利益率
    },
    efficiency: {
      totalAssetTurnover: number,            // 総資本回転率
      tangibleFixedAssetTurnover: number     // 有形固定資産回転率
    },
    safety: {
      currentRatio: number,           // 流動比率
      quickRatio: number,             // 当座比率
      equityRatio: number             // 自己資本比率
    }
  }
}
```

## エラーハンドリング

API 呼び出しでエラーが発生した場合、エラーメッセージと詳細情報が含まれる例外がスローされます。

```javascript
try {
  const result = await FinancialAccountingAPI.Accounts.getAccount('INVALID_CODE');
} catch (error) {
  console.error('エラーが発生しました:', error.message);
  // エラー処理
}
```

## 設定のカスタマイズ

API のベース URL を変更する場合は、`api-client.js` の `API_CONFIG` を編集してください。

```javascript
const API_CONFIG = {
  baseUrl: 'http://your-server:8080',  // ← ここを変更
  headers: {
    'Content-Type': 'application/json',
    'Accept': 'application/json'
  }
};
```

## トラブルシューティング

### CORS エラーが発生する場合

Spring Boot アプリケーション側で CORS 設定が必要です。`OpenApiConfig.java` または専用の CORS 設定クラスを確認してください。

### ネットワークエラーが発生する場合

1. Spring Boot アプリケーションが起動しているか確認
2. `http://localhost:8080/v3/api-docs` にアクセスして OpenAPI 仕様が取得できるか確認
3. ブラウザの開発者ツールでネットワークエラーの詳細を確認

## 関連リンク

- [OpenAPI 仕様書](./openapi.yaml)
- [財務会計システムドキュメント](../../docs/)
- [Script Lab 公式ドキュメント](https://learn.microsoft.com/ja-jp/office/dev/add-ins/overview/explore-with-script-lab)

## ライセンス

Apache 2.0
