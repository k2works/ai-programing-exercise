using Dapper;
using MRS.Application.Ports;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;
using MRS.Infrastructure.Data;

namespace MRS.Infrastructure.Repositories;

/// <summary>
/// Dapperを使用した会議室リポジトリ実装
/// </summary>
public class RoomRepository : IRoomRepository
{
    private readonly IDbConnectionFactory _connectionFactory;

    public RoomRepository(IDbConnectionFactory connectionFactory)
    {
        _connectionFactory = connectionFactory ?? throw new ArgumentNullException(nameof(connectionFactory));
    }

    /// <summary>
    /// 会議室IDで会議室を取得
    /// </summary>
    public async Task<Room?> GetByIdAsync(RoomId roomId, CancellationToken cancellationToken = default)
    {
        if (roomId is null) throw new ArgumentNullException(nameof(roomId));

        const string sql = @"
            SELECT RoomId, RoomName, Capacity, IsActive, CreatedAt, UpdatedAt
            FROM Rooms
            WHERE RoomId = @RoomId";

        using var connection = _connectionFactory.CreateConnection();
        var roomRow = await connection.QueryFirstOrDefaultAsync<RoomRow>(sql, new { RoomId = roomId.Value });

        return roomRow?.ToEntity();
    }

    /// <summary>
    /// 全会議室を取得
    /// </summary>
    public async Task<IEnumerable<Room>> GetAllAsync(CancellationToken cancellationToken = default)
    {
        const string sql = @"
            SELECT RoomId, RoomName, Capacity, IsActive, CreatedAt, UpdatedAt
            FROM Rooms
            ORDER BY RoomName";

        using var connection = _connectionFactory.CreateConnection();
        var roomRows = await connection.QueryAsync<RoomRow>(sql);

        return roomRows.Select(row => row.ToEntity());
    }

    /// <summary>
    /// 指定日の利用可能会議室を取得
    /// </summary>
    public async Task<IEnumerable<ReservableRoom>> GetAvailableRoomsAsync(DateTime date, CancellationToken cancellationToken = default)
    {
        const string sql = @"
            SELECT ReservableRoomId, RoomId, RoomName, IsAvailable, CreatedAt, UpdatedAt
            FROM ReservableRooms
            WHERE IsAvailable = true
            ORDER BY RoomName";

        using var connection = _connectionFactory.CreateConnection();
        var reservableRoomRows = await connection.QueryAsync<ReservableRoomRow>(sql);

        return reservableRoomRows.Select(row => row.ToEntity());
    }

    /// <summary>
    /// 会議室を追加
    /// </summary>
    public async Task AddRoomAsync(Room room, CancellationToken cancellationToken = default)
    {
        if (room is null) throw new ArgumentNullException(nameof(room));

        const string sql = @"
            INSERT INTO Rooms (RoomId, RoomName, Capacity, IsActive, CreatedAt, UpdatedAt)
            VALUES (@RoomId, @RoomName, @Capacity, @IsActive, @CreatedAt, @UpdatedAt)";

        using var connection = _connectionFactory.CreateConnection();
        await connection.ExecuteAsync(sql, new
        {
            RoomId = room.RoomId.Value,
            RoomName = room.RoomName.Value,
            Capacity = room.Capacity,
            IsActive = room.IsActive,
            CreatedAt = room.CreatedAt,
            UpdatedAt = room.UpdatedAt
        });
    }

    /// <summary>
    /// 予約可能会議室を追加
    /// </summary>
    public async Task AddReservableRoomAsync(ReservableRoom reservableRoom, CancellationToken cancellationToken = default)
    {
        if (reservableRoom is null) throw new ArgumentNullException(nameof(reservableRoom));

        const string sql = @"
            INSERT INTO ReservableRooms (ReservableRoomId, RoomId, RoomName, IsAvailable, CreatedAt, UpdatedAt)
            VALUES (@ReservableRoomId, @RoomId, @RoomName, @IsAvailable, @CreatedAt, @UpdatedAt)";

        using var connection = _connectionFactory.CreateConnection();
        await connection.ExecuteAsync(sql, new
        {
            ReservableRoomId = reservableRoom.ReservableRoomId.Value,
            RoomId = reservableRoom.RoomId.Value,
            RoomName = reservableRoom.RoomName.Value,
            IsAvailable = reservableRoom.IsAvailable,
            CreatedAt = reservableRoom.CreatedAt,
            UpdatedAt = reservableRoom.UpdatedAt
        });
    }
}

/// <summary>
/// データベースの会議室テーブル行を表すクラス
/// </summary>
internal class RoomRow
{
    public string RoomId { get; set; } = string.Empty;
    public string RoomName { get; set; } = string.Empty;
    public int Capacity { get; set; }
    public bool IsActive { get; set; }
    public DateTime CreatedAt { get; set; }
    public DateTime UpdatedAt { get; set; }

    /// <summary>
    /// データベース行からドメインエンティティに変換
    /// </summary>
    public Room ToEntity()
    {
        return new Room(
            new RoomId(RoomId),
            new Name(RoomName),
            Capacity,
            IsActive,
            CreatedAt,
            UpdatedAt);
    }
}

/// <summary>
/// データベースの予約可能会議室テーブル行を表すクラス
/// </summary>
internal class ReservableRoomRow
{
    public string ReservableRoomId { get; set; } = string.Empty;
    public string RoomId { get; set; } = string.Empty;
    public string RoomName { get; set; } = string.Empty;
    public bool IsAvailable { get; set; }
    public DateTime CreatedAt { get; set; }
    public DateTime UpdatedAt { get; set; }

    /// <summary>
    /// データベース行からドメインエンティティに変換
    /// </summary>
    public ReservableRoom ToEntity()
    {
        return new ReservableRoom(
            new ReservableRoomId(ReservableRoomId),
            new RoomId(RoomId),
            new Name(RoomName),
            IsAvailable,
            CreatedAt,
            UpdatedAt);
    }
}
