namespace AccountingSystem.Infrastructure.Seed

open System

/// <summary>
/// D社の財務会計システム Seed データを提供するモジュール
/// 8章 D社の事例に基づく勘定科目、仕訳、残高データ
/// </summary>
module AccountingSeedData =

    // 勘定科目開始日(全科目共通)
    let accountStartDate = DateTime(2021, 4, 1)

    /// <summary>
    /// 勘定科目マスタデータ
    /// </summary>
    type AccountData = {
        AccountCode: string
        AccountName: string
        AccountKana: string
        AccountType: string
        IsSummaryAccount: bool
        BsplType: string
        TransactionElement: string
        ExpenseCategory: string
        DisplayOrder: int
    }

    let getAccounts () : AccountData list =
        [
            // 資産の部
            { AccountCode = "1"; AccountName = "資産"; AccountKana = "シサン"; AccountType = "資産"; IsSummaryAccount = true; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 1 }
            { AccountCode = "11"; AccountName = "流動資産"; AccountKana = "リュウドウシサン"; AccountType = "資産"; IsSummaryAccount = true; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 10 }
            { AccountCode = "111"; AccountName = "現金預金"; AccountKana = "ゲンキンヨキン"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 110 }
            { AccountCode = "112"; AccountName = "売掛金"; AccountKana = "ウリカケキン"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 120 }
            { AccountCode = "113"; AccountName = "売上債権"; AccountKana = "ウリアゲサイケン"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 130 }
            { AccountCode = "114"; AccountName = "棚卸資産"; AccountKana = "タナオロシシサン"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 140 }
            { AccountCode = "115"; AccountName = "その他流動資産"; AccountKana = "ソノタリュウドウシサン"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 150 }
            { AccountCode = "12"; AccountName = "固定資産"; AccountKana = "コテイシサン"; AccountType = "資産"; IsSummaryAccount = true; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 200 }
            { AccountCode = "121"; AccountName = "有形固定資産"; AccountKana = "ユウケイコテイシサン"; AccountType = "資産"; IsSummaryAccount = true; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 210 }
            { AccountCode = "1211"; AccountName = "建物及び構築物"; AccountKana = "タテモノオヨビコウチクブツ"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 211 }
            { AccountCode = "1212"; AccountName = "機械装置及び運搬具"; AccountKana = "キカイソウチオヨビウンパング"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 212 }
            { AccountCode = "1213"; AccountName = "工具器具備品"; AccountKana = "コウグキグビヒン"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 213 }
            { AccountCode = "1214"; AccountName = "土地"; AccountKana = "トチ"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 214 }
            { AccountCode = "1215"; AccountName = "その他有形固定資産"; AccountKana = "ソノタユウケイコテイシサン"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 215 }
            { AccountCode = "122"; AccountName = "無形固定資産"; AccountKana = "ムケイコテイシサン"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 220 }
            { AccountCode = "123"; AccountName = "投資その他の資産"; AccountKana = "トウシソノタノシサン"; AccountType = "資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "1"; ExpenseCategory = ""; DisplayOrder = 230 }

            // 負債の部
            { AccountCode = "2"; AccountName = "負債"; AccountKana = "フサイ"; AccountType = "負債"; IsSummaryAccount = true; BsplType = "B"; TransactionElement = "2"; ExpenseCategory = ""; DisplayOrder = 300 }
            { AccountCode = "21"; AccountName = "流動負債"; AccountKana = "リュウドウフサイ"; AccountType = "負債"; IsSummaryAccount = true; BsplType = "B"; TransactionElement = "2"; ExpenseCategory = ""; DisplayOrder = 310 }
            { AccountCode = "211"; AccountName = "買掛金"; AccountKana = "カイカケキン"; AccountType = "負債"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "2"; ExpenseCategory = ""; DisplayOrder = 311 }
            { AccountCode = "212"; AccountName = "短期借入金"; AccountKana = "タンキカリイレキン"; AccountType = "負債"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "2"; ExpenseCategory = ""; DisplayOrder = 312 }
            { AccountCode = "213"; AccountName = "未払金"; AccountKana = "ミバライキン"; AccountType = "負債"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "2"; ExpenseCategory = ""; DisplayOrder = 313 }
            { AccountCode = "214"; AccountName = "未払法人税等"; AccountKana = "ミバライホウジンゼイトウ"; AccountType = "負債"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "2"; ExpenseCategory = ""; DisplayOrder = 314 }
            { AccountCode = "215"; AccountName = "その他流動負債"; AccountKana = "ソノタリュウドウフサイ"; AccountType = "負債"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "2"; ExpenseCategory = ""; DisplayOrder = 315 }
            { AccountCode = "22"; AccountName = "固定負債"; AccountKana = "コテイフサイ"; AccountType = "負債"; IsSummaryAccount = true; BsplType = "B"; TransactionElement = "2"; ExpenseCategory = ""; DisplayOrder = 320 }
            { AccountCode = "221"; AccountName = "長期借入金"; AccountKana = "チョウキカリイレキン"; AccountType = "負債"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "2"; ExpenseCategory = ""; DisplayOrder = 321 }
            { AccountCode = "222"; AccountName = "リース債務"; AccountKana = "リースサイム"; AccountType = "負債"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "2"; ExpenseCategory = ""; DisplayOrder = 322 }

            // 純資産の部
            { AccountCode = "3"; AccountName = "純資産"; AccountKana = "ジュンシサン"; AccountType = "純資産"; IsSummaryAccount = true; BsplType = "B"; TransactionElement = "3"; ExpenseCategory = ""; DisplayOrder = 400 }
            { AccountCode = "31"; AccountName = "資本金"; AccountKana = "シホンキン"; AccountType = "純資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "3"; ExpenseCategory = ""; DisplayOrder = 410 }
            { AccountCode = "32"; AccountName = "資本剰余金"; AccountKana = "シホンジョウヨキン"; AccountType = "純資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "3"; ExpenseCategory = ""; DisplayOrder = 420 }
            { AccountCode = "33"; AccountName = "利益剰余金"; AccountKana = "リエキジョウヨキン"; AccountType = "純資産"; IsSummaryAccount = false; BsplType = "B"; TransactionElement = "3"; ExpenseCategory = ""; DisplayOrder = 430 }

            // 収益の部
            { AccountCode = "4"; AccountName = "収益"; AccountKana = "シュウエキ"; AccountType = "収益"; IsSummaryAccount = true; BsplType = "P"; TransactionElement = "4"; ExpenseCategory = ""; DisplayOrder = 500 }
            { AccountCode = "41"; AccountName = "売上高"; AccountKana = "ウリアゲダカ"; AccountType = "収益"; IsSummaryAccount = false; BsplType = "P"; TransactionElement = "4"; ExpenseCategory = ""; DisplayOrder = 510 }
            { AccountCode = "42"; AccountName = "営業外収益"; AccountKana = "エイギョウガイシュウエキ"; AccountType = "収益"; IsSummaryAccount = false; BsplType = "P"; TransactionElement = "4"; ExpenseCategory = ""; DisplayOrder = 520 }
            { AccountCode = "43"; AccountName = "特別利益"; AccountKana = "トクベツリエキ"; AccountType = "収益"; IsSummaryAccount = false; BsplType = "P"; TransactionElement = "4"; ExpenseCategory = ""; DisplayOrder = 530 }

            // 費用の部
            { AccountCode = "5"; AccountName = "費用"; AccountKana = "ヒヨウ"; AccountType = "費用"; IsSummaryAccount = true; BsplType = "P"; TransactionElement = "5"; ExpenseCategory = ""; DisplayOrder = 600 }
            { AccountCode = "51"; AccountName = "売上原価"; AccountKana = "ウリアゲゲンカ"; AccountType = "費用"; IsSummaryAccount = false; BsplType = "P"; TransactionElement = "5"; ExpenseCategory = "1"; DisplayOrder = 610 }
            { AccountCode = "52"; AccountName = "販売費及び一般管理費"; AccountKana = "ハンバイヒオヨビイッパンカンリヒ"; AccountType = "費用"; IsSummaryAccount = false; BsplType = "P"; TransactionElement = "5"; ExpenseCategory = "2"; DisplayOrder = 620 }
            { AccountCode = "53"; AccountName = "営業外費用"; AccountKana = "エイギョウガイヒヨウ"; AccountType = "費用"; IsSummaryAccount = false; BsplType = "P"; TransactionElement = "5"; ExpenseCategory = ""; DisplayOrder = 630 }
            { AccountCode = "54"; AccountName = "特別損失"; AccountKana = "トクベツソンシツ"; AccountType = "費用"; IsSummaryAccount = false; BsplType = "P"; TransactionElement = "5"; ExpenseCategory = ""; DisplayOrder = 640 }
            { AccountCode = "55"; AccountName = "法人税等"; AccountKana = "ホウジンゼイトウ"; AccountType = "費用"; IsSummaryAccount = false; BsplType = "P"; TransactionElement = "5"; ExpenseCategory = ""; DisplayOrder = 650 }
        ]

    /// <summary>
    /// 勘定科目構成マスタデータ
    /// チルダ(~)連結による階層パスを使用
    /// </summary>
    type AccountStructureData = {
        AccountCode: string
        AccountPath: string
    }

    let getAccountStructures () : AccountStructureData list =
        [
            // 資産の部
            { AccountCode = "1"; AccountPath = "1" }
            { AccountCode = "11"; AccountPath = "1~11" }
            { AccountCode = "111"; AccountPath = "1~11~111" }
            { AccountCode = "112"; AccountPath = "1~11~112" }
            { AccountCode = "113"; AccountPath = "1~11~113" }
            { AccountCode = "114"; AccountPath = "1~11~114" }
            { AccountCode = "115"; AccountPath = "1~11~115" }
            { AccountCode = "12"; AccountPath = "1~12" }
            { AccountCode = "121"; AccountPath = "1~12~121" }
            { AccountCode = "1211"; AccountPath = "1~12~121~1211" }
            { AccountCode = "1212"; AccountPath = "1~12~121~1212" }
            { AccountCode = "1213"; AccountPath = "1~12~121~1213" }
            { AccountCode = "1214"; AccountPath = "1~12~121~1214" }
            { AccountCode = "1215"; AccountPath = "1~12~121~1215" }
            { AccountCode = "122"; AccountPath = "1~12~122" }
            { AccountCode = "123"; AccountPath = "1~12~123" }

            // 負債の部
            { AccountCode = "2"; AccountPath = "2" }
            { AccountCode = "21"; AccountPath = "2~21" }
            { AccountCode = "211"; AccountPath = "2~21~211" }
            { AccountCode = "212"; AccountPath = "2~21~212" }
            { AccountCode = "213"; AccountPath = "2~21~213" }
            { AccountCode = "214"; AccountPath = "2~21~214" }
            { AccountCode = "215"; AccountPath = "2~21~215" }
            { AccountCode = "22"; AccountPath = "2~22" }
            { AccountCode = "221"; AccountPath = "2~22~221" }
            { AccountCode = "222"; AccountPath = "2~22~222" }

            // 純資産・収益・費用の部
            { AccountCode = "3"; AccountPath = "3" }
            { AccountCode = "31"; AccountPath = "3~31" }
            { AccountCode = "32"; AccountPath = "3~32" }
            { AccountCode = "33"; AccountPath = "3~33" }

            { AccountCode = "4"; AccountPath = "4" }
            { AccountCode = "41"; AccountPath = "4~41" }
            { AccountCode = "42"; AccountPath = "4~42" }
            { AccountCode = "43"; AccountPath = "4~43" }

            { AccountCode = "5"; AccountPath = "5" }
            { AccountCode = "51"; AccountPath = "5~51" }
            { AccountCode = "52"; AccountPath = "5~52" }
            { AccountCode = "53"; AccountPath = "5~53" }
            { AccountCode = "54"; AccountPath = "5~54" }
            { AccountCode = "55"; AccountPath = "5~55" }
        ]

    /// <summary>
    /// 日次勘定科目残高データ（令和3年度期末）
    /// D社の貸借対照表データに基づく
    /// </summary>
    type DailyBalanceData = {
        AccountCode: string
        PostingDate: DateTime
        DebitAmount: decimal
        CreditAmount: decimal
    }

    let getFY2021DailyBalances () : DailyBalanceData list =
        let endDate = DateTime(2022, 3, 31)
        [
            // 資産の部（借方に残高）
            { AccountCode = "111"; PostingDate = endDate; DebitAmount = 593256m; CreditAmount = 0m }    // 現金預金
            { AccountCode = "112"; PostingDate = endDate; DebitAmount = 1085840m; CreditAmount = 0m }   // 売掛金
            { AccountCode = "114"; PostingDate = endDate; DebitAmount = 948537m; CreditAmount = 0m }    // 棚卸資産
            { AccountCode = "115"; PostingDate = endDate; DebitAmount = 48560m; CreditAmount = 0m }     // その他流動資産
            { AccountCode = "1211"; PostingDate = endDate; DebitAmount = 64524m; CreditAmount = 0m }    // 建物及び構築物
            { AccountCode = "122"; PostingDate = endDate; DebitAmount = 37492m; CreditAmount = 0m }     // 無形固定資産
            { AccountCode = "123"; PostingDate = endDate; DebitAmount = 84957m; CreditAmount = 0m }     // 投資その他の資産

            // 負債の部（貸方に残高）
            { AccountCode = "211"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 191034m }    // 買掛金
            { AccountCode = "212"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 120000m }    // 短期借入金
            { AccountCode = "213"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 197262m }    // 未払金
            { AccountCode = "214"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 250114m }    // 未払法人税等
            { AccountCode = "215"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 92984m }     // その他流動負債
            { AccountCode = "221"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 22500m }     // 長期借入金

            // 純資産の部（貸方に残高）
            { AccountCode = "31"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 100000m }     // 資本金
            { AccountCode = "33"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 1889272m }    // 利益剰余金

            // 収益の部（貸方に残高）
            { AccountCode = "41"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 5796105m }    // 売上高
            { AccountCode = "42"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 368m }        // 営業外収益

            // 費用の部（借方に残高）
            { AccountCode = "51"; PostingDate = endDate; DebitAmount = 2185856m; CreditAmount = 0m }    // 売上原価
            { AccountCode = "52"; PostingDate = endDate; DebitAmount = 2625222m; CreditAmount = 0m }    // 販売費及び一般管理費
            { AccountCode = "53"; PostingDate = endDate; DebitAmount = 2676m; CreditAmount = 0m }       // 営業外費用
            { AccountCode = "55"; PostingDate = endDate; DebitAmount = 331059m; CreditAmount = 0m }     // 法人税等
        ]

    let getFY2022DailyBalances () : DailyBalanceData list =
        let endDate = DateTime(2023, 3, 31)
        [
            // 資産の部（借方に残高）
            { AccountCode = "111"; PostingDate = endDate; DebitAmount = 1133270m; CreditAmount = 0m }   // 現金預金
            { AccountCode = "112"; PostingDate = endDate; DebitAmount = 864915m; CreditAmount = 0m }    // 売掛金
            { AccountCode = "114"; PostingDate = endDate; DebitAmount = 740810m; CreditAmount = 0m }    // 棚卸資産
            { AccountCode = "115"; PostingDate = endDate; DebitAmount = 38550m; CreditAmount = 0m }     // その他流動資産
            { AccountCode = "1211"; PostingDate = endDate; DebitAmount = 63256m; CreditAmount = 0m }    // 建物及び構築物
            { AccountCode = "122"; PostingDate = endDate; DebitAmount = 34683m; CreditAmount = 0m }     // 無形固定資産
            { AccountCode = "123"; PostingDate = endDate; DebitAmount = 99415m; CreditAmount = 0m }     // 投資その他の資産

            // 負債の部（貸方に残高）
            { AccountCode = "211"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 197162m }    // 買掛金
            { AccountCode = "212"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 70000m }     // 短期借入金
            { AccountCode = "213"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 104341m }    // 未払金
            { AccountCode = "214"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 184887m }    // 未払法人税等
            { AccountCode = "215"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 84123m }     // その他流動負債
            { AccountCode = "221"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 24360m }     // 長期借入金
            { AccountCode = "222"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 2793m }      // リース債務

            // 純資産の部（貸方に残高）
            { AccountCode = "31"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 100000m }     // 資本金
            { AccountCode = "33"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 2207233m }    // 利益剰余金

            // 収益の部（貸方に残高）
            { AccountCode = "41"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 4547908m }    // 売上高
            { AccountCode = "42"; PostingDate = endDate; DebitAmount = 0m; CreditAmount = 11608m }      // 営業外収益

            // 費用の部（借方に残高）
            { AccountCode = "51"; PostingDate = endDate; DebitAmount = 1743821m; CreditAmount = 0m }    // 売上原価
            { AccountCode = "52"; PostingDate = endDate; DebitAmount = 2277050m; CreditAmount = 0m }    // 販売費及び一般管理費
            { AccountCode = "53"; PostingDate = endDate; DebitAmount = 1613m; CreditAmount = 0m }       // 営業外費用
            { AccountCode = "55"; PostingDate = endDate; DebitAmount = 169072m; CreditAmount = 0m }     // 法人税等
        ]

    /// <summary>
    /// D社の財務指標（期待値）
    /// </summary>
    type ExpectedFinancialRatios = {
        FiscalYear: int
        GrossProfitMargin: decimal       // 売上高総利益率
        OperatingProfitMargin: decimal   // 売上高営業利益率
        OrdinaryProfitMargin: decimal    // 売上高経常利益率
        SellingExpenseRatio: decimal     // 売上高販管費比率
        TotalAssetTurnover: decimal      // 総資本回転率
        AccountsReceivableTurnover: decimal // 売上債権回転率
        InventoryTurnover: decimal       // 棚卸資産回転率
        TangibleFixedAssetTurnover: decimal // 有形固定資産回転率
        CurrentRatio: decimal            // 流動比率
        QuickRatio: decimal              // 当座比率
        FixedRatio: decimal              // 固定比率
        FixedLongTermSuitabilityRatio: decimal // 固定長期適合率
        DebtRatio: decimal               // 負債比率
        EquityRatio: decimal             // 自己資本比率
    }

    let getExpectedRatios () : ExpectedFinancialRatios list =
        [
            // 令和3年度
            {
                FiscalYear = 2021
                GrossProfitMargin = 62.29m
                OperatingProfitMargin = 16.99m
                OrdinaryProfitMargin = 16.95m
                SellingExpenseRatio = 45.29m
                TotalAssetTurnover = 2.02m
                AccountsReceivableTurnover = 5.34m
                InventoryTurnover = 6.11m
                TangibleFixedAssetTurnover = 89.83m
                CurrentRatio = 314.33m
                QuickRatio = 197.22m
                FixedRatio = 9.40m
                FixedLongTermSuitabilityRatio = 9.29m
                DebtRatio = 43.93m
                EquityRatio = 69.48m
            }
            // 令和4年度
            {
                FiscalYear = 2022
                GrossProfitMargin = 61.66m
                OperatingProfitMargin = 11.59m
                OrdinaryProfitMargin = 11.81m
                SellingExpenseRatio = 50.07m
                TotalAssetTurnover = 1.53m
                AccountsReceivableTurnover = 5.26m
                InventoryTurnover = 6.14m
                TangibleFixedAssetTurnover = 71.90m
                CurrentRatio = 433.64m
                QuickRatio = 311.97m
                FixedRatio = 8.55m
                FixedLongTermSuitabilityRatio = 8.45m
                DebtRatio = 28.94m
                EquityRatio = 77.56m
            }
        ]
