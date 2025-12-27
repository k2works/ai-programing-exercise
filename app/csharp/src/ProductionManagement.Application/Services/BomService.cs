using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;

namespace ProductionManagement.Application.Services;

/// <summary>
/// BOM アプリケーションサービス
/// </summary>
public class BomService
{
    private readonly IBomRepository _bomRepository;
    private readonly IItemRepository _itemRepository;

    public BomService(IBomRepository bomRepository, IItemRepository itemRepository)
    {
        _bomRepository = bomRepository;
        _itemRepository = itemRepository;
    }

    /// <summary>
    /// 品目の部品展開（再帰的に子品目を取得）
    /// </summary>
    public async Task<BomNode> ExplodeBomAsync(string parentItemCode, int level = 0)
    {
        var item = await _itemRepository.FindByItemCodeAsync(parentItemCode)
            ?? throw new ItemNotFoundException(parentItemCode);

        var children = await _bomRepository.FindByParentItemCodeAsync(parentItemCode);
        var childNodes = new List<BomNode>();

        foreach (var child in children)
        {
            var childNode = await ExplodeBomAsync(child.ChildItemCode, level + 1);
            childNodes.Add(childNode with { RequiredQuantity = child.RequiredQuantity });
        }

        return new BomNode(
            ItemCode: item.ItemCode,
            ItemName: item.ItemName,
            RequiredQuantity: 1m,
            Level: level,
            Children: childNodes
        );
    }

    /// <summary>
    /// 逆展開（使用先照会）
    /// </summary>
    public async Task<IReadOnlyList<WhereUsedResult>> WhereUsedAsync(string childItemCode)
    {
        var parents = await _bomRepository.FindByChildItemCodeAsync(childItemCode);
        var results = new List<WhereUsedResult>();

        foreach (var bom in parents)
        {
            var parentItem = await _itemRepository.FindByItemCodeAsync(bom.ParentItemCode);
            if (parentItem is not null)
            {
                results.Add(new WhereUsedResult(
                    ParentItemCode: bom.ParentItemCode,
                    ItemName: parentItem.ItemName,
                    RequiredQuantity: bom.RequiredQuantity
                ));
            }
        }

        return results;
    }
}

/// <summary>
/// BOM 階層ノード
/// </summary>
public record BomNode(
    string ItemCode,
    string ItemName,
    decimal RequiredQuantity,
    int Level,
    IReadOnlyList<BomNode> Children
);

/// <summary>
/// 使用先照会結果
/// </summary>
public record WhereUsedResult(
    string ParentItemCode,
    string ItemName,
    decimal RequiredQuantity
);
