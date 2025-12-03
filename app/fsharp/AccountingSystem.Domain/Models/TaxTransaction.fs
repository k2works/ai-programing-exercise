namespace AccountingSystem.Domain.Models

/// <summary>
/// 課税取引エンティティ
/// </summary>
[<CLIMutable>]
type TaxTransaction = {
    TaxCode: string                    // 課税取引コード
    TaxName: string                    // 課税取引名
    TaxRate: decimal                   // 税率
    Description: string option         // 説明
}

/// TaxTransaction エンティティのファクトリ関数
module TaxTransaction =
    let create taxCode taxName taxRate =
        {
            TaxCode = taxCode
            TaxName = taxName
            TaxRate = taxRate
            Description = None
        }
