namespace ProductionManagement.WPF.ViewModels.Items;

/// <summary>
/// 編集モード
/// </summary>
public enum EditMode
{
    Create,
    Edit
}

/// <summary>
/// 編集画面パラメーター
/// </summary>
public record ItemEditParameter(
    EditMode Mode,
    string? ItemCode = null
);
