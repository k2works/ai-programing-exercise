using MRS.Application.DTOs.Rooms;
using MRS.Application.Ports;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Application.Services;

/// <summary>
/// 会議室サービス
/// </summary>
public class RoomService : IRoomService
{
    private readonly IRoomRepository _roomRepository;

    public RoomService(IRoomRepository roomRepository)
    {
        _roomRepository = roomRepository ?? throw new ArgumentNullException(nameof(roomRepository));
    }

    /// <summary>
    /// 利用可能な会議室を取得
    /// </summary>
    public async Task<IEnumerable<ReservableRoomDto>> GetAvailableRoomsAsync(
        GetRoomsRequestDto request, 
        CancellationToken cancellationToken = default)
    {
        var targetDate = request.Date ?? DateTime.Today;
        var reservableRooms = await _roomRepository.GetAvailableRoomsAsync(targetDate, cancellationToken);

        var result = new List<ReservableRoomDto>();

        foreach (var reservableRoom in reservableRooms)
        {
            // 利用可能状態のフィルタリング
            if (request.AvailableOnly && !reservableRoom.IsAvailable)
                continue;

            // 会議室情報を取得
            var room = await _roomRepository.GetByIdAsync(reservableRoom.RoomId, cancellationToken);
            if (room == null)
                continue;

            // 最小収容人数でのフィルタリング
            if (request.MinCapacity.HasValue && room.Capacity < request.MinCapacity.Value)
                continue;

            result.Add(new ReservableRoomDto
            {
                ReservableRoomId = reservableRoom.ReservableRoomId.Value,
                RoomId = reservableRoom.RoomId.Value,
                RoomName = room.RoomName.Value,
                Capacity = room.Capacity,
                IsAvailable = reservableRoom.IsAvailable,
                Date = targetDate
            });
        }

        return result;
    }

    /// <summary>
    /// 会議室IDで会議室を取得
    /// </summary>
    public async Task<RoomDto> GetRoomByIdAsync(string roomId, CancellationToken cancellationToken = default)
    {
        var room = await _roomRepository.GetByIdAsync(new RoomId(roomId), cancellationToken);
        
        if (room == null)
            throw new ArgumentException($"会議室が見つかりません: {roomId}");

        return new RoomDto
        {
            RoomId = room.RoomId.Value,
            RoomName = room.RoomName.Value,
            Capacity = room.Capacity,
            IsActive = room.IsActive
        };
    }

    /// <summary>
    /// すべての会議室を取得
    /// </summary>
    public async Task<IEnumerable<RoomDto>> GetAllRoomsAsync(CancellationToken cancellationToken = default)
    {
        var rooms = await _roomRepository.GetAllAsync(cancellationToken);

        return rooms.Select(room => new RoomDto
        {
            RoomId = room.RoomId.Value,
            RoomName = room.RoomName.Value,
            Capacity = room.Capacity,
            IsActive = room.IsActive
        });
    }
}
