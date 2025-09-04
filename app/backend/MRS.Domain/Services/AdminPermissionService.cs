using MRS.Domain.Entities;

namespace MRS.Domain.Services;

/// <summary>
/// 管理者権限を管理するドメインサービス
/// </summary>
public class AdminPermissionService
{
    /// <summary>
    /// ユーザーが他者の予約をキャンセルできるかを判定します
    /// </summary>
    /// <param name="user">権限を確認するユーザー</param>
    /// <returns>他者の予約をキャンセルできる場合true</returns>
    /// <exception cref="ArgumentNullException">ユーザーがnullの場合</exception>
    public bool CanCancelOthersReservations(User user)
    {
        ArgumentNullException.ThrowIfNull(user);
        
        return user.Role == UserRole.Admin;
    }
    
    /// <summary>
    /// ユーザーが指定した予約を変更できるかを判定します
    /// </summary>
    /// <param name="user">権限を確認するユーザー</param>
    /// <param name="reservation">対象の予約</param>
    /// <returns>予約を変更できる場合true</returns>
    /// <exception cref="ArgumentNullException">ユーザーまたは予約がnullの場合</exception>
    public bool CanModifyReservation(User user, Reservation reservation)
    {
        ArgumentNullException.ThrowIfNull(user);
        ArgumentNullException.ThrowIfNull(reservation);
        
        // 管理者は全ての予約を変更可能
        if (user.Role == UserRole.Admin)
            return true;
            
        // 予約の所有者は自分の予約を変更可能
        return reservation.UserId == user.UserId.Value;
    }
    
    /// <summary>
    /// 管理者操作の権限を検証します
    /// </summary>
    /// <param name="user">権限を確認するユーザー</param>
    /// <param name="operationName">操作名</param>
    /// <exception cref="ArgumentNullException">ユーザーがnullの場合</exception>
    /// <exception cref="UnauthorizedAccessException">管理者権限がない場合</exception>
    public void ValidateAdminOperation(User user, string operationName)
    {
        ArgumentNullException.ThrowIfNull(user);
        
        if (user.Role != UserRole.Admin)
        {
            throw new UnauthorizedAccessException(
                $"{operationName}を実行するには管理者権限が必要です。現在の権限: {user.Role}");
        }
    }
    
    /// <summary>
    /// ユーザーが管理者権限を持っているかを判定します
    /// </summary>
    /// <param name="user">権限を確認するユーザー</param>
    /// <returns>管理者権限を持っている場合true</returns>
    /// <exception cref="ArgumentNullException">ユーザーがnullの場合</exception>
    public bool IsAdmin(User user)
    {
        ArgumentNullException.ThrowIfNull(user);
        
        return user.Role == UserRole.Admin;
    }
    
    /// <summary>
    /// ユーザーが指定した予約の所有者かを判定します
    /// </summary>
    /// <param name="user">権限を確認するユーザー</param>
    /// <param name="reservation">対象の予約</param>
    /// <returns>予約の所有者である場合true</returns>
    /// <exception cref="ArgumentNullException">ユーザーまたは予約がnullの場合</exception>
    public bool IsReservationOwner(User user, Reservation reservation)
    {
        ArgumentNullException.ThrowIfNull(user);
        ArgumentNullException.ThrowIfNull(reservation);
        
        return reservation.UserId == user.UserId.Value;
    }
}