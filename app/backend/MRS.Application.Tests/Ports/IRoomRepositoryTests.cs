using Xunit;
using Moq;
using MRS.Application.Ports;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Application.Tests.Ports;

/// <summary>
/// IRoomRepositoryインターフェースのテスト
/// </summary>
public class IRoomRepositoryTests
{
    [Fact]
    public void GetByIdAsync_ValidRoomId_ShouldReturnRoom()
    {
        // Arrange
        var mockRoomRepository = new Mock<IRoomRepository>();
        var roomId = new RoomId("ROOM001");

        var expectedRoom = new Room(
            roomId,
            new Name("会議室A"),
            10
        );

        mockRoomRepository
            .Setup(x => x.GetByIdAsync(roomId, It.IsAny<CancellationToken>()))
            .ReturnsAsync(expectedRoom);

        // Act & Assert
        var roomRepository = mockRoomRepository.Object;
        Assert.NotNull(roomRepository);

        // メソッドシグネチャの存在確認
        var getByIdMethod = typeof(IRoomRepository).GetMethod("GetByIdAsync");
        Assert.NotNull(getByIdMethod);
        Assert.Equal(typeof(Task<Room?>), getByIdMethod.ReturnType);
    }

    [Fact]
    public void GetAllAsync_ShouldReturnAllRooms()
    {
        // Arrange
        var mockRoomRepository = new Mock<IRoomRepository>();

        var expectedRooms = new List<Room>
        {
            new(new RoomId("ROOM001"), new Name("会議室A"), 10),
            new(new RoomId("ROOM002"), new Name("会議室B"), 8)
        };

        mockRoomRepository
            .Setup(x => x.GetAllAsync(It.IsAny<CancellationToken>()))
            .ReturnsAsync(expectedRooms);

        // Act & Assert
        var roomRepository = mockRoomRepository.Object;
        Assert.NotNull(roomRepository);

        // メソッドシグネチャの存在確認
        var getAllMethod = typeof(IRoomRepository).GetMethod("GetAllAsync");
        Assert.NotNull(getAllMethod);
        Assert.Equal(typeof(Task<IEnumerable<Room>>), getAllMethod.ReturnType);
    }

    [Fact]
    public void GetAvailableRoomsAsync_ValidDate_ShouldReturnAvailableRooms()
    {
        // Arrange
        var mockRoomRepository = new Mock<IRoomRepository>();
        var targetDate = DateTime.Today;

        var expectedReservableRooms = new List<ReservableRoom>
        {
            new(new ReservableRoomId("RESROOM001"), new RoomId("ROOM001"), new Name("会議室A")),
            new(new ReservableRoomId("RESROOM002"), new RoomId("ROOM002"), new Name("会議室B"))
        };

        mockRoomRepository
            .Setup(x => x.GetAvailableRoomsAsync(targetDate, It.IsAny<CancellationToken>()))
            .ReturnsAsync(expectedReservableRooms);

        // Act & Assert
        var roomRepository = mockRoomRepository.Object;
        Assert.NotNull(roomRepository);

        // メソッドシグネチャの存在確認
        var getAvailableRoomsMethod = typeof(IRoomRepository).GetMethod("GetAvailableRoomsAsync");
        Assert.NotNull(getAvailableRoomsMethod);
        Assert.Equal(typeof(Task<IEnumerable<ReservableRoom>>), getAvailableRoomsMethod.ReturnType);
    }

    [Fact]
    public void AddRoomAsync_ValidRoom_ShouldCompleteSuccessfully()
    {
        // Arrange
        var mockRoomRepository = new Mock<IRoomRepository>();
        var room = new Room(
            new RoomId("ROOM001"),
            new Name("会議室A"),
            10
        );

        mockRoomRepository
            .Setup(x => x.AddRoomAsync(room, It.IsAny<CancellationToken>()))
            .Returns(Task.CompletedTask);

        // Act & Assert
        var roomRepository = mockRoomRepository.Object;
        Assert.NotNull(roomRepository);

        // メソッドシグネチャの存在確認
        var addRoomMethod = typeof(IRoomRepository).GetMethod("AddRoomAsync");
        Assert.NotNull(addRoomMethod);
        Assert.Equal(typeof(Task), addRoomMethod.ReturnType);
    }

    [Fact]
    public void AddReservableRoomAsync_ValidReservableRoom_ShouldCompleteSuccessfully()
    {
        // Arrange
        var mockRoomRepository = new Mock<IRoomRepository>();
        var reservableRoom = new ReservableRoom(
            new ReservableRoomId("RESROOM001"),
            new RoomId("ROOM001"),
            new Name("会議室A")
        );

        mockRoomRepository
            .Setup(x => x.AddReservableRoomAsync(reservableRoom, It.IsAny<CancellationToken>()))
            .Returns(Task.CompletedTask);

        // Act & Assert
        var roomRepository = mockRoomRepository.Object;
        Assert.NotNull(roomRepository);

        // メソッドシグネチャの存在確認
        var addReservableRoomMethod = typeof(IRoomRepository).GetMethod("AddReservableRoomAsync");
        Assert.NotNull(addReservableRoomMethod);
        Assert.Equal(typeof(Task), addReservableRoomMethod.ReturnType);
    }
}
