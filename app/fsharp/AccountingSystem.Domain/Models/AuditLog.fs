namespace AccountingSystem.Domain.Models

open System
open AccountingSystem.Domain.Types

/// <summary>
/// 監査ログドメインモデル（不変）
/// 財務会計システムにおける法的要件（5W1H）を満たすため、
/// すべての重要な操作を記録します。
/// </summary>
type AuditLog = {
    /// 監査ログID（主キー）
    Id: int64 option
    /// エンティティ種別（例: "Account", "Journal"）
    EntityType: string
    /// エンティティID（例: "1001", "J-2024-0001"）
    EntityId: string
    /// 操作種別
    Action: AuditAction
    /// 操作ユーザーID
    UserId: string
    /// 操作ユーザー名
    UserName: string
    /// 発生日時
    Timestamp: DateTime
    /// 変更前の値（JSON形式）
    OldValues: string option
    /// 変更後の値（JSON形式）
    NewValues: string option
    /// 変更内容（JSON形式）
    Changes: string option
    /// 変更理由
    Reason: string option
    /// IPアドレス
    IpAddress: string option
    /// ユーザーエージェント
    UserAgent: string option
}

module AuditLog =
    /// <summary>
    /// CREATE操作用のファクトリ関数
    /// </summary>
    let create
        (entityType: string)
        (entityId: string)
        (userId: string)
        (userName: string)
        (changes: string)
        (ipAddress: string option) : AuditLog =
        {
            Id = None
            EntityType = entityType
            EntityId = entityId
            Action = AuditAction.Create
            UserId = userId
            UserName = userName
            Timestamp = DateTime.UtcNow
            OldValues = None
            NewValues = Some changes
            Changes = Some changes
            Reason = None
            IpAddress = ipAddress
            UserAgent = None
        }

    /// <summary>
    /// UPDATE操作用のファクトリ関数
    /// </summary>
    let createForUpdate
        (entityType: string)
        (entityId: string)
        (userId: string)
        (userName: string)
        (oldValues: string)
        (newValues: string)
        (ipAddress: string option) : AuditLog =
        {
            Id = None
            EntityType = entityType
            EntityId = entityId
            Action = AuditAction.Update
            UserId = userId
            UserName = userName
            Timestamp = DateTime.UtcNow
            OldValues = Some oldValues
            NewValues = Some newValues
            Changes = None
            Reason = None
            IpAddress = ipAddress
            UserAgent = None
        }

    /// <summary>
    /// DELETE操作用のファクトリ関数
    /// </summary>
    let createForDelete
        (entityType: string)
        (entityId: string)
        (userId: string)
        (userName: string)
        (oldValues: string)
        (reason: string option)
        (ipAddress: string option) : AuditLog =
        {
            Id = None
            EntityType = entityType
            EntityId = entityId
            Action = AuditAction.Delete
            UserId = userId
            UserName = userName
            Timestamp = DateTime.UtcNow
            OldValues = Some oldValues
            NewValues = None
            Changes = None
            Reason = reason
            IpAddress = ipAddress
            UserAgent = None
        }

    /// <summary>
    /// サマリー文字列を生成
    /// </summary>
    let getSummary (auditLog: AuditLog) : string =
        $"{auditLog.EntityType} {auditLog.EntityId} を{AuditAction.getDisplayName auditLog.Action}"
