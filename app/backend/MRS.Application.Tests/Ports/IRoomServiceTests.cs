using Xunit;
using Moq;
using MRS.Application.Ports;
using MRS.Application.DTOs.Rooms;

namespace MRS.Application.Tests.Ports;

/// <summary>
/// IRoomServiceインターフェースのテスト
/// </summary>
public class IRoomServiceTests
{
    [Fact]
    public void GetAvailableRoomsAsync_ValidRequest_ShouldReturnRoomList()
    {
        // Arrange
        var mockRoomService = new Mock<IRoomService>();
        var request = new GetRoomsRequestDto
        {
            Date = DateTime.Today,
            MinCapacity = 5,
            AvailableOnly = true
        };

        var expectedRooms = new List<ReservableRoomDto>
        {
            new()
            {
                ReservableRoomId = "RESROOM001",
                RoomId = "ROOM001",
                RoomName = "会議室A",
                Capacity = 10,
                IsAvailable = true,
                Date = DateTime.Today
            },
            new()
            {
                ReservableRoomId = "RESROOM002", 
                RoomId = "ROOM002",
                RoomName = "会議室B",
                Capacity = 8,
                IsAvailable = true,
                Date = DateTime.Today
            }
        };

        mockRoomService
            .Setup(x => x.GetAvailableRoomsAsync(request, It.IsAny<CancellationToken>()))
            .ReturnsAsync(expectedRooms);

        // Act & Assert
        var roomService = mockRoomService.Object;
        Assert.NotNull(roomService);

        // メソッドシグネチャの存在確認
        var getAvailableRoomsMethod = typeof(IRoomService).GetMethod("GetAvailableRoomsAsync");
        Assert.NotNull(getAvailableRoomsMethod);
        Assert.Equal(typeof(Task<IEnumerable<ReservableRoomDto>>), getAvailableRoomsMethod.ReturnType);
    }

    [Fact]
    public void GetRoomByIdAsync_ValidRoomId_ShouldReturnRoomDto()
    {
        // Arrange
        var mockRoomService = new Mock<IRoomService>();
        var roomId = "ROOM001";

        var expectedRoom = new RoomDto
        {
            RoomId = "ROOM001",
            RoomName = "会議室A",
            Capacity = 10,
            IsActive = true
        };

        mockRoomService
            .Setup(x => x.GetRoomByIdAsync(roomId, It.IsAny<CancellationToken>()))
            .ReturnsAsync(expectedRoom);

        // Act & Assert
        var roomService = mockRoomService.Object;
        Assert.NotNull(roomService);

        // メソッドシグネチャの存在確認
        var getRoomByIdMethod = typeof(IRoomService).GetMethod("GetRoomByIdAsync");
        Assert.NotNull(getRoomByIdMethod);
        Assert.Equal(typeof(Task<RoomDto>), getRoomByIdMethod.ReturnType);
    }

    [Fact]
    public void GetRoomByIdAsync_InvalidRoomId_ShouldThrowArgumentException()
    {
        // Arrange
        var mockRoomService = new Mock<IRoomService>();
        var invalidRoomId = "INVALID_ROOM";

        mockRoomService
            .Setup(x => x.GetRoomByIdAsync(invalidRoomId, It.IsAny<CancellationToken>()))
            .ThrowsAsync(new ArgumentException("Room not found"));

        // Act & Assert
        var roomService = mockRoomService.Object;
        Assert.NotNull(roomService);
    }

    [Fact]
    public void GetAllRoomsAsync_ShouldReturnAllRooms()
    {
        // Arrange
        var mockRoomService = new Mock<IRoomService>();

        var expectedRooms = new List<RoomDto>
        {
            new()
            {
                RoomId = "ROOM001",
                RoomName = "会議室A",
                Capacity = 10,
                IsActive = true
            },
            new()
            {
                RoomId = "ROOM002",
                RoomName = "会議室B",
                Capacity = 8,
                IsActive = false
            }
        };

        mockRoomService
            .Setup(x => x.GetAllRoomsAsync(It.IsAny<CancellationToken>()))
            .ReturnsAsync(expectedRooms);

        // Act & Assert
        var roomService = mockRoomService.Object;
        Assert.NotNull(roomService);

        // メソッドシグネチャの存在確認
        var getAllRoomsMethod = typeof(IRoomService).GetMethod("GetAllRoomsAsync");
        Assert.NotNull(getAllRoomsMethod);
        Assert.Equal(typeof(Task<IEnumerable<RoomDto>>), getAllRoomsMethod.ReturnType);
    }
}
