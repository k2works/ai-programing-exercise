module AccountingSystem.Domain.Models.Journal

open System
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Models.JournalLineItem
open AccountingSystem.Domain.Models.JournalLine

/// <summary>
/// 仕訳エンティティ（3層構造のヘッダー・集約ルート）
/// 伝票全体の属性と明細を管理
/// </summary>
type Journal = {
    /// 仕訳伝票番号（エンティティID）
    VoucherNumber: VoucherNumber
    /// 起票日（実際の取引発生日）
    PostingDate: DateTime
    /// 入力日（システム入力日）
    EntryDate: DateTime
    /// 決算仕訳フラグ
    SettlementFlag: SettlementFlag
    /// 単振フラグ（true: 単一仕訳、false: 複合仕訳）
    IsSingleEntry: bool
    /// 仕訳伝票区分
    VoucherType: VoucherType
    /// 定期計上フラグ
    IsRecurring: bool
    /// 社員コード
    EmployeeCode: string option
    /// 部門コード
    DepartmentCode: string option
    /// 赤伝フラグ
    RedSlipFlag: RedSlipFlag
    /// 赤黒伝票番号（対応する赤伝票/黒伝票の番号）
    RedBlackVoucherNumber: int option
    /// 仕訳明細（2層目）
    Lines: JournalLine list
}

module Journal =
    /// 新規仕訳を作成
    let create voucherNumber postingDate entryDate voucherType =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            PostingDate = postingDate
            EntryDate = entryDate
            SettlementFlag = SettlementFlag.Normal
            IsSingleEntry = false
            VoucherType = voucherType
            IsRecurring = false
            EmployeeCode = None
            DepartmentCode = None
            RedSlipFlag = RedSlipFlag.Normal
            RedBlackVoucherNumber = None
            Lines = []
        }

    /// 仕訳を明細付きで作成
    let createWithLines voucherNumber postingDate entryDate voucherType lines =
        { create voucherNumber postingDate entryDate voucherType with Lines = lines }

    /// 決算仕訳を作成
    let createSettlement voucherNumber postingDate entryDate =
        { create voucherNumber postingDate entryDate Transfer with
            SettlementFlag = SettlementFlag.Settlement }

    /// 赤伝票を作成
    let createRedSlip voucherNumber postingDate entryDate originalVoucherNumber =
        { create voucherNumber postingDate entryDate Transfer with
            RedSlipFlag = RedSlipFlag.RedSlip
            RedBlackVoucherNumber = Some originalVoucherNumber }

    /// エンティティの同一性判定（VoucherNumber で判定）
    let equal (a: Journal) (b: Journal) =
        a.VoucherNumber.Number = b.VoucherNumber.Number

    /// ハッシュコード
    let hashCode (journal: Journal) =
        journal.VoucherNumber.Number.GetHashCode()

    /// 赤伝票かどうか
    let isRedSlip (journal: Journal) =
        match journal.RedSlipFlag with
        | RedSlipFlag.RedSlip -> true
        | RedSlipFlag.Normal -> false

    /// 決算仕訳かどうか
    let isSettlement (journal: Journal) =
        match journal.SettlementFlag with
        | SettlementFlag.Settlement -> true
        | SettlementFlag.Normal -> false

    /// 全明細の貸借明細を取得
    let getAllItems (journal: Journal) =
        journal.Lines |> List.collect (fun line -> line.Items)

    /// 借方合計を計算
    let sumDebit (journal: Journal) =
        getAllItems journal |> JournalLineItem.sumDebit

    /// 貸方合計を計算
    let sumCredit (journal: Journal) =
        getAllItems journal |> JournalLineItem.sumCredit

    /// 貸借バランスを検証（複式簿記の原理）
    let validateBalance (journal: Journal) =
        getAllItems journal |> JournalLineItem.validateBalance
