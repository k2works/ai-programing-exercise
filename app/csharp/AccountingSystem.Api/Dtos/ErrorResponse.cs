namespace AccountingSystem.Api.Dtos;

/// <summary>
/// エラーレスポンス DTO
/// </summary>
public class ErrorResponse
{
    /// <summary>
    /// HTTP ステータスコード
    /// </summary>
    public int Status { get; set; }

    /// <summary>
    /// エラー種別
    /// </summary>
    public string Error { get; set; } = string.Empty;

    /// <summary>
    /// エラーメッセージ
    /// </summary>
    public string Message { get; set; } = string.Empty;

    /// <summary>
    /// 詳細情報（バリデーションエラーなど）
    /// </summary>
    public List<string>? Details { get; set; }

    /// <summary>
    /// エラー発生日時
    /// </summary>
    public DateTime Timestamp { get; set; }
}
