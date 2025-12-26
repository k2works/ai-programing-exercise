using System.ComponentModel.DataAnnotations.Schema;

namespace ProductionManagement.Domain.Models;

/// <summary>
/// 品目マスタ
/// </summary>
public class Item
{
    public int Id { get; set; }

    [Column("品目コード")]
    public required string ItemCode { get; set; }

    [Column("適用開始日")]
    public DateOnly EffectiveFrom { get; set; }

    [Column("適用停止日")]
    public DateOnly? EffectiveTo { get; set; }

    [Column("品名")]
    public required string ItemName { get; set; }

    [Column("品目区分")]
    public required string ItemCategory { get; set; }

    [Column("単位コード")]
    public string? UnitCode { get; set; }

    [Column("リードタイム")]
    public int LeadTime { get; set; }

    [Column("安全リードタイム")]
    public int SafetyLeadTime { get; set; }

    [Column("安全在庫数")]
    public decimal SafetyStock { get; set; }

    [Column("歩留率")]
    public decimal YieldRate { get; set; } = 100m;

    [Column("最小ロット数")]
    public decimal MinLotSize { get; set; } = 1m;

    [Column("刻みロット数")]
    public decimal LotIncrement { get; set; } = 1m;

    [Column("最大ロット数")]
    public decimal? MaxLotSize { get; set; }

    [Column("有効期間")]
    public int? ShelfLife { get; set; }

    [Column("作成日時")]
    public DateTime CreatedAt { get; set; }

    [Column("更新日時")]
    public DateTime UpdatedAt { get; set; }
}
