using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Application.Ports;

/// <summary>
/// ユーザーリポジトリのポート（出力）
/// </summary>
public interface IUserRepository
{
    /// <summary>
    /// ユーザーIDでユーザーを取得
    /// </summary>
    /// <param name="userId">ユーザーID</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>ユーザーエンティティ（見つからない場合はnull）</returns>
    Task<User?> GetByIdAsync(UserId userId, CancellationToken cancellationToken = default);

    /// <summary>
    /// ユーザーを追加
    /// </summary>
    /// <param name="user">追加するユーザー</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>完了タスク</returns>
    Task AddAsync(User user, CancellationToken cancellationToken = default);

    /// <summary>
    /// ユーザーを更新
    /// </summary>
    /// <param name="user">更新するユーザー</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>完了タスク</returns>
    Task UpdateAsync(User user, CancellationToken cancellationToken = default);

    /// <summary>
    /// ユーザーを削除
    /// </summary>
    /// <param name="userId">削除するユーザーID</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>完了タスク</returns>
    Task DeleteAsync(UserId userId, CancellationToken cancellationToken = default);
}
