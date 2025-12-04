namespace AccountingSystem.Api.Dtos

open System

/// <summary>
/// エラーレスポンス DTO
/// </summary>
[<CLIMutable>]
type ErrorResponse = {
    /// HTTP ステータスコード
    Status: int
    /// エラー種別
    Error: string
    /// エラーメッセージ
    Message: string
    /// タイムスタンプ
    Timestamp: DateTime
}

module ErrorResponse =
    /// <summary>
    /// エラーレスポンスを作成
    /// </summary>
    let create status error message =
        {
            Status = status
            Error = error
            Message = message
            Timestamp = DateTime.UtcNow
        }
