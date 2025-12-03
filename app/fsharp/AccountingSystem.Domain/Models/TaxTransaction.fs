namespace AccountingSystem.Domain.Models

/// <summary>
/// 課税取引エンティティ
/// </summary>
type TaxTransaction = {
    TaxCode: string                    // 課税取引コード
    TaxName: string                    // 課税取引名
    TaxRate: decimal                   // 税率
    Description: string option         // 説明
}

/// TaxTransaction エンティティのファクトリ関数とユーティリティ
module TaxTransaction =
    /// <summary>
    /// 新規 TaxTransaction エンティティを作成
    /// </summary>
    let create taxCode taxName taxRate =
        {
            TaxCode = taxCode
            TaxName = taxName
            TaxRate = taxRate
            Description = None
        }

    /// <summary>
    /// エンティティ同一性の判定（TaxCode による識別）
    /// </summary>
    let equal (a: TaxTransaction) (b: TaxTransaction) =
        a.TaxCode = b.TaxCode

    /// <summary>
    /// エンティティのハッシュコードを取得
    /// </summary>
    let hashCode (taxTransaction: TaxTransaction) =
        taxTransaction.TaxCode.GetHashCode()
