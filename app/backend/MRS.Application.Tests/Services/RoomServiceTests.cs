using Xunit;
using Moq;
using MRS.Application.Services;
using MRS.Application.Ports;
using MRS.Application.DTOs.Rooms;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Application.Tests.Services;

/// <summary>
/// RoomServiceのテスト
/// </summary>
public class RoomServiceTests
{
    private readonly Mock<IRoomRepository> _roomRepositoryMock;
    private readonly RoomService _roomService;

    public RoomServiceTests()
    {
        _roomRepositoryMock = new Mock<IRoomRepository>();
        _roomService = new RoomService(_roomRepositoryMock.Object);
    }

    [Fact]
    public async Task GetAvailableRoomsAsync_ValidRequest_ShouldReturnFilteredRooms()
    {
        // Arrange
        var request = new GetRoomsRequestDto
        {
            Date = DateTime.Today,
            MinCapacity = 5,
            AvailableOnly = true
        };

        var reservableRooms = new List<ReservableRoom>
        {
            new(new ReservableRoomId("RESROOM001"), new RoomId("ROOM001"), new Name("会議室A")),
            new(new ReservableRoomId("RESROOM002"), new RoomId("ROOM002"), new Name("会議室B")),
            new(new ReservableRoomId("RESROOM003"), new RoomId("ROOM003"), new Name("会議室C"))
        };

        // 会議室Cを利用不可にする
        reservableRooms[2].MakeUnavailable();

        var rooms = new List<Room>
        {
            new(new RoomId("ROOM001"), new Name("会議室A"), 10),
            new(new RoomId("ROOM002"), new Name("会議室B"), 4), // 最小収容人数未満
            new(new RoomId("ROOM003"), new Name("会議室C"), 8)
        };

        _roomRepositoryMock
            .Setup(x => x.GetAvailableRoomsAsync(request.Date ?? DateTime.Today, It.IsAny<CancellationToken>()))
            .ReturnsAsync(reservableRooms);

        _roomRepositoryMock
            .Setup(x => x.GetByIdAsync(It.IsAny<RoomId>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync((RoomId roomId, CancellationToken _) => 
                rooms.FirstOrDefault(r => r.RoomId.Equals(roomId)));

        // Act
        var result = await _roomService.GetAvailableRoomsAsync(request, CancellationToken.None);

        // Assert
        var resultList = result.ToList();
        Assert.Single(resultList); // 条件を満たすのは会議室Aのみ
        
        var roomDto = resultList.First();
        Assert.Equal("RESROOM001", roomDto.ReservableRoomId);
        Assert.Equal("ROOM001", roomDto.RoomId);
        Assert.Equal("会議室A", roomDto.RoomName);
        Assert.Equal(10, roomDto.Capacity);
        Assert.True(roomDto.IsAvailable);
        Assert.Equal(DateTime.Today, roomDto.Date);

        _roomRepositoryMock.Verify(x => x.GetAvailableRoomsAsync(It.IsAny<DateTime>(), It.IsAny<CancellationToken>()), Times.Once);
    }

    [Fact]
    public async Task GetAvailableRoomsAsync_NoDateSpecified_ShouldUseTodayAsDefault()
    {
        // Arrange
        var request = new GetRoomsRequestDto
        {
            Date = null, // 日付未指定
            AvailableOnly = true
        };

        _roomRepositoryMock
            .Setup(x => x.GetAvailableRoomsAsync(DateTime.Today, It.IsAny<CancellationToken>()))
            .ReturnsAsync(new List<ReservableRoom>());

        // Act
        await _roomService.GetAvailableRoomsAsync(request, CancellationToken.None);

        // Assert
        _roomRepositoryMock.Verify(x => x.GetAvailableRoomsAsync(DateTime.Today, It.IsAny<CancellationToken>()), Times.Once);
    }

    [Fact]
    public async Task GetRoomByIdAsync_ValidRoomId_ShouldReturnRoomDto()
    {
        // Arrange
        var roomId = "ROOM001";
        var room = new Room(
            new RoomId(roomId),
            new Name("会議室A"),
            10
        );

        _roomRepositoryMock
            .Setup(x => x.GetByIdAsync(It.IsAny<RoomId>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(room);

        // Act
        var result = await _roomService.GetRoomByIdAsync(roomId, CancellationToken.None);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(roomId, result.RoomId);
        Assert.Equal("会議室A", result.RoomName);
        Assert.Equal(10, result.Capacity);
        Assert.True(result.IsActive);

        _roomRepositoryMock.Verify(x => x.GetByIdAsync(It.IsAny<RoomId>(), It.IsAny<CancellationToken>()), Times.Once);
    }

    [Fact]
    public async Task GetRoomByIdAsync_RoomNotFound_ShouldThrowArgumentException()
    {
        // Arrange
        var roomId = "INVALID_ROOM";

        _roomRepositoryMock
            .Setup(x => x.GetByIdAsync(It.IsAny<RoomId>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync((Room?)null);

        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(() => 
            _roomService.GetRoomByIdAsync(roomId, CancellationToken.None));

        _roomRepositoryMock.Verify(x => x.GetByIdAsync(It.IsAny<RoomId>(), It.IsAny<CancellationToken>()), Times.Once);
    }

    [Fact]
    public async Task GetAllRoomsAsync_ShouldReturnAllRooms()
    {
        // Arrange
        var rooms = new List<Room>
        {
            new(new RoomId("ROOM001"), new Name("会議室A"), 10),
            new(new RoomId("ROOM002"), new Name("会議室B"), 8),
            new(new RoomId("ROOM003"), new Name("会議室C"), 6)
        };

        // 会議室Cを非アクティブ化
        rooms[2].Deactivate();

        _roomRepositoryMock
            .Setup(x => x.GetAllAsync(It.IsAny<CancellationToken>()))
            .ReturnsAsync(rooms);

        // Act
        var result = await _roomService.GetAllRoomsAsync(CancellationToken.None);

        // Assert
        var resultList = result.ToList();
        Assert.Equal(3, resultList.Count);

        var room1 = resultList.First(r => r.RoomId == "ROOM001");
        Assert.Equal("会議室A", room1.RoomName);
        Assert.Equal(10, room1.Capacity);
        Assert.True(room1.IsActive);

        var room3 = resultList.First(r => r.RoomId == "ROOM003");
        Assert.Equal("会議室C", room3.RoomName);
        Assert.False(room3.IsActive); // 非アクティブ

        _roomRepositoryMock.Verify(x => x.GetAllAsync(It.IsAny<CancellationToken>()), Times.Once);
    }

    [Fact]
    public async Task GetAvailableRoomsAsync_AvailableOnlyFalse_ShouldIncludeUnavailableRooms()
    {
        // Arrange
        var request = new GetRoomsRequestDto
        {
            Date = DateTime.Today,
            AvailableOnly = false // 利用不可も含める
        };

        var reservableRooms = new List<ReservableRoom>
        {
            new(new ReservableRoomId("RESROOM001"), new RoomId("ROOM001"), new Name("会議室A")),
            new(new ReservableRoomId("RESROOM002"), new RoomId("ROOM002"), new Name("会議室B"))
        };

        // 会議室Bを利用不可にする
        reservableRooms[1].MakeUnavailable();

        var rooms = new List<Room>
        {
            new(new RoomId("ROOM001"), new Name("会議室A"), 10),
            new(new RoomId("ROOM002"), new Name("会議室B"), 8)
        };

        _roomRepositoryMock
            .Setup(x => x.GetAvailableRoomsAsync(request.Date ?? DateTime.Today, It.IsAny<CancellationToken>()))
            .ReturnsAsync(reservableRooms);

        _roomRepositoryMock
            .Setup(x => x.GetByIdAsync(It.IsAny<RoomId>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync((RoomId roomId, CancellationToken _) => 
                rooms.FirstOrDefault(r => r.RoomId.Equals(roomId)));

        // Act
        var result = await _roomService.GetAvailableRoomsAsync(request, CancellationToken.None);

        // Assert
        var resultList = result.ToList();
        Assert.Equal(2, resultList.Count); // 利用不可も含めて2件

        var availableRoom = resultList.First(r => r.IsAvailable);
        var unavailableRoom = resultList.First(r => !r.IsAvailable);
        
        Assert.Equal("会議室A", availableRoom.RoomName);
        Assert.Equal("会議室B", unavailableRoom.RoomName);
    }
}
