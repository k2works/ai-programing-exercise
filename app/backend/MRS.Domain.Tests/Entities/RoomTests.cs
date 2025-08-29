using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.Entities;

public class RoomTests
{
    [Fact]
    public void CtorValidParametersCreatesRoom()
    {
        // Arrange
        var roomId = new RoomId("room001");
        var roomName = new Name("大会議室A");

        // Act
        var room = new Room(roomId, roomName);

        // Assert
        Assert.NotNull(room);
        Assert.Equal(roomId, room.RoomId);
        Assert.Equal(roomName, room.RoomName);
        Assert.Equal(10, room.Capacity); // デフォルト容量
        Assert.True(room.IsActive); // デフォルトでアクティブ
        Assert.True(room.CreatedAt <= DateTime.UtcNow);
        Assert.True(room.UpdatedAt <= DateTime.UtcNow);
    }

    [Fact]
    public void CtorWithCapacityCreatesRoomWithSpecifiedCapacity()
    {
        // Arrange
        var roomId = new RoomId("room002");
        var roomName = new Name("小会議室B");
        const int capacity = 6;

        // Act
        var room = new Room(roomId, roomName, capacity);

        // Assert
        Assert.Equal(capacity, room.Capacity);
    }

    [Fact]
    public void CtorNullRoomIdThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new Room(null!, new Name("会議室")));
    }

    [Fact]
    public void CtorNullRoomNameThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new Room(new RoomId("room001"), null!));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(-1)]
    [InlineData(101)] // 最大100名を超過
    public void CtorInvalidCapacityThrowsArgumentException(int invalidCapacity)
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => new Room(new RoomId("room001"), new Name("会議室"), invalidCapacity));
    }

    [Fact]
    public void ChangeRoomNameUpdatesNameAndTimestamp()
    {
        // Arrange
        var room = CreateTestRoom();
        var newName = new Name("変更後会議室名");
        var originalUpdatedAt = room.UpdatedAt;

        // 時間の経過をシミュレート
        Thread.Sleep(1);

        // Act
        room.ChangeRoomName(newName);

        // Assert
        Assert.Equal(newName, room.RoomName);
        Assert.True(room.UpdatedAt > originalUpdatedAt);
    }

    [Fact]
    public void ChangeRoomNameNullNameThrowsArgumentNullException()
    {
        // Arrange
        var room = CreateTestRoom();

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => room.ChangeRoomName(null!));
    }

    [Fact]
    public void ChangeCapacityUpdatesCapacityAndTimestamp()
    {
        // Arrange
        var room = CreateTestRoom();
        const int newCapacity = 20;
        var originalUpdatedAt = room.UpdatedAt;

        // 時間の経過をシミュレート
        Thread.Sleep(1);

        // Act
        room.ChangeCapacity(newCapacity);

        // Assert
        Assert.Equal(newCapacity, room.Capacity);
        Assert.True(room.UpdatedAt > originalUpdatedAt);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(-1)]
    [InlineData(101)]
    public void ChangeCapacityInvalidCapacityThrowsArgumentException(int invalidCapacity)
    {
        // Arrange
        var room = CreateTestRoom();

        // Act & Assert
        Assert.Throws<ArgumentException>(() => room.ChangeCapacity(invalidCapacity));
    }

    [Fact]
    public void DeactivateRoomSetsIsActiveToFalseAndUpdatesTimestamp()
    {
        // Arrange
        var room = CreateTestRoom();
        var originalUpdatedAt = room.UpdatedAt;

        // 時間の経過をシミュレート
        Thread.Sleep(1);

        // Act
        room.Deactivate();

        // Assert
        Assert.False(room.IsActive);
        Assert.True(room.UpdatedAt > originalUpdatedAt);
    }

    [Fact]
    public void ActivateRoomSetsIsActiveToTrueAndUpdatesTimestamp()
    {
        // Arrange
        var room = CreateTestRoom();
        room.Deactivate(); // 一度非アクティブにする
        var originalUpdatedAt = room.UpdatedAt;

        // 時間の経過をシミュレート
        Thread.Sleep(1);

        // Act
        room.Activate();

        // Assert
        Assert.True(room.IsActive);
        Assert.True(room.UpdatedAt > originalUpdatedAt);
    }

    [Fact]
    public void EqualsSameRoomIdReturnsTrue()
    {
        // Arrange
        var roomId = new RoomId("room001");
        var room1 = new Room(roomId, new Name("会議室A"));
        var room2 = new Room(roomId, new Name("会議室B")); // 異なる名前

        // Act & Assert
        Assert.Equal(room1, room2); // RoomIdが同じなら等価
        Assert.True(room1.Equals(room2));
        Assert.True(room1 == room2);
        Assert.False(room1 != room2);
    }

    [Fact]
    public void EqualsDifferentRoomIdReturnsFalse()
    {
        // Arrange
        var room1 = new Room(new RoomId("room001"), new Name("会議室A"));
        var room2 = new Room(new RoomId("room002"), new Name("会議室A"));

        // Act & Assert
        Assert.NotEqual(room1, room2);
        Assert.False(room1.Equals(room2));
        Assert.False(room1 == room2);
        Assert.True(room1 != room2);
    }

    [Fact]
    public void GetHashCodeSameRoomIdReturnsSameHashCode()
    {
        // Arrange
        var roomId = new RoomId("room001");
        var room1 = new Room(roomId, new Name("会議室A"));
        var room2 = new Room(roomId, new Name("会議室B"));

        // Act & Assert
        Assert.Equal(room1.GetHashCode(), room2.GetHashCode());
    }

    [Fact]
    public void ToStringReturnsRoomInfo()
    {
        // Arrange
        var room = CreateTestRoom(roomId: "room001", roomName: "大会議室A", capacity: 15);

        // Act
        var result = room.ToString();

        // Assert
        Assert.Contains("room001", result);
        Assert.Contains("大会議室A", result);
        Assert.Contains("15", result);
    }

    [Fact]
    public void CanAccommodateReturnsTrueWhenCapacityIsSufficient()
    {
        // Arrange
        var room = CreateTestRoom(capacity: 10);

        // Act & Assert
        Assert.True(room.CanAccommodate(5));
        Assert.True(room.CanAccommodate(10)); // 境界値
    }

    [Fact]
    public void CanAccommodateReturnsFalseWhenCapacityIsInsufficient()
    {
        // Arrange
        var room = CreateTestRoom(capacity: 10);

        // Act & Assert
        Assert.False(room.CanAccommodate(11));
        Assert.False(room.CanAccommodate(0)); // 0人以下は無効
        Assert.False(room.CanAccommodate(-1));
    }

    [Fact]
    public void CanAccommodateReturnsFalseWhenRoomIsInactive()
    {
        // Arrange
        var room = CreateTestRoom(capacity: 10);
        room.Deactivate();

        // Act & Assert
        Assert.False(room.CanAccommodate(5)); // 非アクティブなら利用不可
    }

    private static Room CreateTestRoom(string roomId = "testroom", string roomName = "テスト会議室", int capacity = 10)
    {
        return new Room(
            new RoomId(roomId),
            new Name(roomName),
            capacity
        );
    }
}
