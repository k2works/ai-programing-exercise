/**
 * 財務会計システム API クライアント for Excel Script Lab
 *
 * このクライアントは Excel Script Lab で使用できる JavaScript API クライアントです。
 * OpenAPI 仕様（openapi.yaml）に基づいて生成されました。
 *
 * @see openapi.yaml OpenAPI 仕様書
 */

// ================================================================================
// Configuration
// ================================================================================

const API_CONFIG = {
  baseUrl: 'http://localhost:8080',
  headers: {
    'Content-Type': 'application/json',
    'Accept': 'application/json'
  }
};

// ================================================================================
// Utility Functions
// ================================================================================

/**
 * HTTP リクエストを実行する汎用関数
 * @param {string} method - HTTP メソッド (GET, POST, PUT, DELETE)
 * @param {string} path - API パス
 * @param {Object} options - リクエストオプション
 * @returns {Promise<Object>} レスポンスデータ
 */
async function request(method, path, options = {}) {
  const url = `${API_CONFIG.baseUrl}${path}`;
  const config = {
    method,
    headers: { ...API_CONFIG.baseUrl, ...options.headers },
    ...options
  };

  if (options.body) {
    config.body = JSON.stringify(options.body);
  }

  try {
    const response = await fetch(url, config);

    if (!response.ok) {
      const error = await response.json().catch(() => ({}));
      throw new Error(`API Error: ${response.status} ${response.statusText} - ${JSON.stringify(error)}`);
    }

    // 204 No Content の場合は null を返す
    if (response.status === 204) {
      return null;
    }

    return await response.json();
  } catch (error) {
    console.error('API Request Failed:', error);
    throw error;
  }
}

/**
 * クエリパラメータをURLエンコードする
 * @param {Object} params - クエリパラメータ
 * @returns {string} エンコードされたクエリ文字列
 */
function buildQueryString(params) {
  const query = new URLSearchParams();
  Object.entries(params).forEach(([key, value]) => {
    if (value !== undefined && value !== null) {
      if (Array.isArray(value)) {
        value.forEach(v => query.append(key, v));
      } else {
        query.set(key, value);
      }
    }
  });
  return query.toString() ? `?${query.toString()}` : '';
}

// ================================================================================
// API Client: 勘定科目マスタ管理 API
// ================================================================================

const AccountsAPI = {
  /**
   * 勘定科目一覧取得
   * @returns {Promise<Array>} 勘定科目のリスト
   */
  async getAllAccounts() {
    return request('GET', '/api/v1/accounts');
  },

  /**
   * 勘定科目取得
   * @param {string} accountCode - 勘定科目コード
   * @returns {Promise<Object>} 勘定科目データ
   */
  async getAccount(accountCode) {
    return request('GET', `/api/v1/accounts/${accountCode}`);
  },

  /**
   * 勘定科目作成
   * @param {Object} accountData - 勘定科目データ
   * @returns {Promise<Object>} 作成された勘定科目
   */
  async createAccount(accountData) {
    return request('POST', '/api/v1/accounts', { body: accountData });
  },

  /**
   * 勘定科目更新
   * @param {string} accountCode - 勘定科目コード
   * @param {Object} accountData - 更新する勘定科目データ
   * @returns {Promise<Object>} 更新された勘定科目
   */
  async updateAccount(accountCode, accountData) {
    return request('PUT', `/api/v1/accounts/${accountCode}`, { body: accountData });
  },

  /**
   * 勘定科目削除
   * @param {string} accountCode - 勘定科目コード
   * @returns {Promise<null>} 削除成功時は null
   */
  async deleteAccount(accountCode) {
    return request('DELETE', `/api/v1/accounts/${accountCode}`);
  }
};

// ================================================================================
// API Client: 仕訳管理 API
// ================================================================================

const JournalsAPI = {
  /**
   * 仕訳一覧取得
   * @returns {Promise<Array>} 仕訳のリスト
   */
  async getAllJournals() {
    return request('GET', '/api/v1/journals');
  },

  /**
   * 仕訳取得
   * @param {string} journalNo - 仕訳番号
   * @returns {Promise<Object>} 仕訳データ
   */
  async getJournal(journalNo) {
    return request('GET', `/api/v1/journals/${journalNo}`);
  },

  /**
   * 仕訳作成
   * @param {Object} journalData - 仕訳データ
   * @returns {Promise<Object>} 作成された仕訳
   */
  async createJournal(journalData) {
    return request('POST', '/api/v1/journals', { body: journalData });
  },

  /**
   * 仕訳更新
   * @param {string} journalNo - 仕訳番号
   * @param {Object} journalData - 更新する仕訳データ
   * @returns {Promise<Object>} 更新された仕訳
   */
  async updateJournal(journalNo, journalData) {
    return request('PUT', `/api/v1/journals/${journalNo}`, { body: journalData });
  },

  /**
   * 仕訳削除
   * @param {string} journalNo - 仕訳番号
   * @returns {Promise<null>} 削除成功時は null
   */
  async deleteJournal(journalNo) {
    return request('DELETE', `/api/v1/journals/${journalNo}`);
  }
};

// ================================================================================
// API Client: イベントソーシング版仕訳 API
// ================================================================================

const JournalEntriesEventSourcingAPI = {
  /**
   * 仕訳作成（イベントソーシング）
   * @param {Object} entryData - 仕訳データ
   * @returns {Promise<Object>} 作成された仕訳（ID含む）
   */
  async createJournalEntry(entryData) {
    return request('POST', '/api/v1/journal-entries-es', { body: entryData });
  },

  /**
   * 仕訳取得（イベント再生）
   * @param {string} id - 仕訳ID
   * @returns {Promise<Object>} 仕訳データ
   */
  async getJournalEntry(id) {
    return request('GET', `/api/v1/journal-entries-es/${id}`);
  },

  /**
   * 仕訳承認
   * @param {string} id - 仕訳ID
   * @param {Object} approvalData - 承認データ（承認者、コメント）
   * @returns {Promise<Object>} 承認結果
   */
  async approveJournalEntry(id, approvalData) {
    return request('POST', `/api/v1/journal-entries-es/${id}/approve`, { body: approvalData });
  },

  /**
   * 仕訳削除
   * @param {string} id - 仕訳ID
   * @param {Object} deleteData - 削除理由とユーザーID
   * @returns {Promise<Object>} 削除結果
   */
  async deleteJournalEntry(id, deleteData) {
    return request('DELETE', `/api/v1/journal-entries-es/${id}`, { body: deleteData });
  }
};

// ================================================================================
// API Client: 財務諸表 API
// ================================================================================

const FinancialStatementsAPI = {
  /**
   * 貸借対照表取得
   * @param {string} asOfDate - 基準日（YYYY-MM-DD形式、省略時は本日）
   * @returns {Promise<Object>} 貸借対照表データ
   */
  async getBalanceSheet(asOfDate = null) {
    const query = buildQueryString({ asOfDate });
    return request('GET', `/api/v1/financial-statements/balance-sheet${query}`);
  },

  /**
   * 損益計算書取得
   * @param {string} fromDate - 開始日（YYYY-MM-DD形式、省略時は当月1日）
   * @param {string} toDate - 終了日（YYYY-MM-DD形式、省略時は本日）
   * @returns {Promise<Object>} 損益計算書データ
   */
  async getIncomeStatement(fromDate = null, toDate = null) {
    const query = buildQueryString({ fromDate, toDate });
    return request('GET', `/api/v1/financial-statements/income-statement${query}`);
  },

  /**
   * 財務指標取得
   * @param {string} asOfDate - 貸借対照表基準日（YYYY-MM-DD形式、省略時は本日）
   * @param {string} fromDate - 損益計算書開始日（YYYY-MM-DD形式、省略時は当月1日）
   * @param {string} toDate - 損益計算書終了日（YYYY-MM-DD形式、省略時は本日）
   * @returns {Promise<Object>} 財務指標データ
   */
  async getFinancialRatios(asOfDate = null, fromDate = null, toDate = null) {
    const query = buildQueryString({ asOfDate, fromDate, toDate });
    return request('GET', `/api/v1/financial-statements/financial-ratios${query}`);
  }
};

// ================================================================================
// API Client: 財務分析 API
// ================================================================================

const FinancialAnalysisAPI = {
  /**
   * 会計年度別財務分析
   * @param {number} fiscalYear - 会計年度
   * @returns {Promise<Object>} 財務分析結果
   */
  async analyzeByFiscalYear(fiscalYear) {
    return request('GET', `/api/v1/financial-analysis/${fiscalYear}`);
  },

  /**
   * 複数期間比較分析
   * @param {Array<number>} fiscalYears - 比較する会計年度の配列
   * @returns {Promise<Object>} 比較分析結果
   */
  async compareMultiplePeriods(fiscalYears) {
    const query = buildQueryString({ fiscalYears });
    return request('GET', `/api/v1/financial-analysis/compare${query}`);
  }
};

// ================================================================================
// API Client: 監査ログ API
// ================================================================================

const AuditLogsAPI = {
  /**
   * 期間別監査ログ取得
   * @param {string} startDate - 開始日時（ISO 8601形式）
   * @param {string} endDate - 終了日時（ISO 8601形式）
   * @param {number} limit - 取得件数上限（デフォルト: 100）
   * @returns {Promise<Array>} 監査ログのリスト
   */
  async getAuditLogsByPeriod(startDate, endDate, limit = 100) {
    const query = buildQueryString({ startDate, endDate, limit });
    return request('GET', `/api/v1/audit-logs/period${query}`);
  },

  /**
   * ユーザー操作履歴取得
   * @param {string} userId - ユーザーID
   * @param {string} startDate - 開始日時（ISO 8601形式）
   * @param {string} endDate - 終了日時（ISO 8601形式）
   * @returns {Promise<Array>} 監査ログのリスト
   */
  async getUserActivity(userId, startDate, endDate) {
    const query = buildQueryString({ startDate, endDate });
    return request('GET', `/api/v1/audit-logs/user/${userId}${query}`);
  },

  /**
   * エンティティ変更履歴取得
   * @param {string} entityType - エンティティタイプ
   * @param {string} entityId - エンティティID
   * @returns {Promise<Array>} 監査ログのリスト
   */
  async getEntityHistory(entityType, entityId) {
    return request('GET', `/api/v1/audit-logs/entity/${entityType}/${entityId}`);
  }
};

// ================================================================================
// Excel Script Lab Integration Helper Functions
// ================================================================================

/**
 * Excel テーブルから勘定科目データを取得する
 * @param {Excel.Worksheet} worksheet - ワークシート
 * @param {string} tableName - テーブル名
 * @returns {Array<Object>} 勘定科目データの配列
 */
function getAccountsFromTable(worksheet, tableName) {
  const table = worksheet.tables.getItem(tableName);
  const dataRange = table.getDataBodyRange();
  const values = dataRange.values;

  return values.map(row => ({
    accountCode: row[0],
    accountName: row[1],
    accountAbbr: row[2] || null,
    accountKana: row[3] || null,
    bsplType: row[4],
    debitCreditType: row[5] || null,
    elementType: row[6] || null,
    displayOrder: row[7] ? parseInt(row[7]) : null
  }));
}

/**
 * 財務分析結果を Excel テーブルに書き込む
 * @param {Excel.Worksheet} worksheet - ワークシート
 * @param {Object} analysisResult - 財務分析結果
 * @param {string} startCell - 開始セル（例: "A1"）
 */
function writeAnalysisResultToSheet(worksheet, analysisResult, startCell = "A1") {
  const range = worksheet.getRange(startCell);

  // ヘッダー
  const headers = [
    ["会計年度", analysisResult.fiscalYear],
    [""],
    ["財務データ", ""],
    ["売上高", analysisResult.financialData.sales],
    ["営業利益", analysisResult.financialData.operatingProfit],
    ["総資産", analysisResult.financialData.totalAssets],
    [""],
    ["収益性指標", ""],
    ["売上高営業利益率", analysisResult.ratios.profitability.operatingProfitMargin],
    [""],
    ["効率性指標", ""],
    ["総資本回転率", analysisResult.ratios.efficiency.totalAssetTurnover],
    ["有形固定資産回転率", analysisResult.ratios.efficiency.tangibleFixedAssetTurnover],
    [""],
    ["安全性指標", ""],
    ["流動比率", analysisResult.ratios.safety.currentRatio],
    ["当座比率", analysisResult.ratios.safety.quickRatio],
    ["自己資本比率", analysisResult.ratios.safety.equityRatio]
  ];

  const outputRange = range.getResizedRange(headers.length - 1, 1);
  outputRange.values = headers;
  outputRange.format.autofitColumns();
}

/**
 * 貸借対照表を Excel に書き込む
 * @param {Excel.Worksheet} worksheet - ワークシート
 * @param {Object} balanceSheet - 貸借対照表データ
 * @param {string} startCell - 開始セル（例: "A1"）
 */
function writeBalanceSheetToSheet(worksheet, balanceSheet, startCell = "A1") {
  const range = worksheet.getRange(startCell);

  const data = [
    ["貸借対照表", "", "基準日:", balanceSheet.asOfDate],
    [""],
    ["資産の部", "", "負債・純資産の部", ""],
    ["科目コード", "科目名", "残高", "構成比"]
  ];

  // 資産
  balanceSheet.assets.forEach(item => {
    data.push([item.accountCode, item.accountName, item.balance, item.ratio]);
  });

  data.push(["", "資産合計", balanceSheet.totalAssets, ""]);
  data.push([""]);

  // 負債
  data.push(["負債の部", "", "", ""]);
  balanceSheet.liabilities.forEach(item => {
    data.push([item.accountCode, item.accountName, item.balance, item.ratio]);
  });

  data.push(["", "負債合計", balanceSheet.totalLiabilities, ""]);
  data.push([""]);

  // 純資産
  data.push(["純資産の部", "", "", ""]);
  balanceSheet.equity.forEach(item => {
    data.push([item.accountCode, item.accountName, item.balance, item.ratio]);
  });

  data.push(["", "純資産合計", balanceSheet.totalEquity, ""]);
  data.push(["", "負債・純資産合計", balanceSheet.totalLiabilitiesAndEquity, ""]);

  const outputRange = range.getResizedRange(data.length - 1, 3);
  outputRange.values = data;
  outputRange.format.autofitColumns();
}

// ================================================================================
// Export API Client
// ================================================================================

// Script Lab でグローバルスコープに公開
if (typeof window !== 'undefined') {
  window.FinancialAccountingAPI = {
    // API クライアント
    Accounts: AccountsAPI,
    Journals: JournalsAPI,
    JournalEntriesES: JournalEntriesEventSourcingAPI,
    FinancialStatements: FinancialStatementsAPI,
    FinancialAnalysis: FinancialAnalysisAPI,
    AuditLogs: AuditLogsAPI,

    // Excel 統合ヘルパー
    Excel: {
      getAccountsFromTable,
      writeAnalysisResultToSheet,
      writeBalanceSheetToSheet
    },

    // 設定
    config: API_CONFIG
  };
}
