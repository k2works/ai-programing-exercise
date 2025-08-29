using System.Net;
using System.Net.Http.Json;
using System.Text.Json;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Moq;
using MRS.Application.Ports;
using MRS.Application.DTOs.Rooms;

namespace MRS.Api.Tests.Controllers;

public class RoomsControllerTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly WebApplicationFactory<Program> _factory;
    private readonly HttpClient _client;
    private readonly Mock<IRoomService> _mockRoomService;
    private readonly JsonSerializerOptions _jsonOptions;

    public RoomsControllerTests(WebApplicationFactory<Program> factory)
    {
        _mockRoomService = new Mock<IRoomService>();
        
        _jsonOptions = new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
        };
        
        _factory = factory.WithWebHostBuilder(builder =>
        {
            builder.ConfigureServices(services =>
            {
                // Remove existing IRoomService registration
                var descriptor = services.SingleOrDefault(
                    d => d.ServiceType == typeof(IRoomService));
                if (descriptor != null)
                {
                    services.Remove(descriptor);
                }

                // Add mock IRoomService
                services.AddSingleton(_mockRoomService.Object);
            });
        });
        
        _client = _factory.CreateClient();
    }

    [Fact]
    public async Task GetAllRooms_ShouldReturnRoomsList()
    {
        // Arrange
        var expectedRooms = new List<RoomDto>
        {
            new RoomDto
            {
                RoomId = "room-001",
                RoomName = "会議室A",
                Capacity = 10,
                IsActive = true
            },
            new RoomDto
            {
                RoomId = "room-002", 
                RoomName = "会議室B",
                Capacity = 20,
                IsActive = true
            }
        };

        _mockRoomService
            .Setup(x => x.GetAllRoomsAsync(CancellationToken.None))
            .ReturnsAsync(expectedRooms);

        // Act
        var response = await _client.GetAsync("/api/rooms");

        // Assert
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        
        var content = await response.Content.ReadAsStringAsync();
        var actualRooms = JsonSerializer.Deserialize<List<RoomDto>>(content, _jsonOptions);

        Assert.NotNull(actualRooms);
        Assert.Equal(2, actualRooms.Count);
        Assert.Equal(expectedRooms[0].RoomId, actualRooms[0].RoomId);
        Assert.Equal(expectedRooms[1].RoomName, actualRooms[1].RoomName);
    }

    [Fact]
    public async Task GetRoomById_ValidId_ShouldReturnRoom()
    {
        // Arrange
        var roomId = "room-001";
        var expectedRoom = new RoomDto
        {
            RoomId = roomId,
            RoomName = "会議室A",
            Capacity = 10,
            IsActive = true
        };

        _mockRoomService
            .Setup(x => x.GetRoomByIdAsync(roomId, CancellationToken.None))
            .ReturnsAsync(expectedRoom);

        // Act
        var response = await _client.GetAsync($"/api/rooms/{roomId}");

        // Assert
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        
        var content = await response.Content.ReadAsStringAsync();
        var actualRoom = JsonSerializer.Deserialize<RoomDto>(content, _jsonOptions);

        Assert.NotNull(actualRoom);
        Assert.Equal(expectedRoom.RoomId, actualRoom.RoomId);
        Assert.Equal(expectedRoom.RoomName, actualRoom.RoomName);
        Assert.Equal(expectedRoom.Capacity, actualRoom.Capacity);
    }

    [Fact]
    public async Task GetRoomById_InvalidId_ShouldReturnNotFound()
    {
        // Arrange
        var roomId = "non-existent";

        _mockRoomService
            .Setup(x => x.GetRoomByIdAsync(roomId, CancellationToken.None))
            .ThrowsAsync(new ArgumentException("Room not found"));

        // Act
        var response = await _client.GetAsync($"/api/rooms/{roomId}");

        // Assert
        Assert.Equal(HttpStatusCode.NotFound, response.StatusCode);
    }

    [Fact]
    public async Task GetAvailableRooms_WithFilters_ShouldReturnFilteredRooms()
    {
        // Arrange
        var requestDate = DateTime.Today.AddDays(1);
        var minCapacity = 10;
        
        var expectedRooms = new List<ReservableRoomDto>
        {
            new ReservableRoomDto
            {
                ReservableRoomId = "reservable-001",
                RoomId = "room-001",
                RoomName = "会議室A",
                Capacity = 10,
                IsAvailable = true,
                Date = requestDate
            }
        };

        _mockRoomService
            .Setup(x => x.GetAvailableRoomsAsync(It.IsAny<GetRoomsRequestDto>(), CancellationToken.None))
            .ReturnsAsync(expectedRooms);

        // Act
        var response = await _client.GetAsync($"/api/rooms/available?date={requestDate:yyyy-MM-dd}&minCapacity={minCapacity}&availableOnly=true");

        // Assert
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        
        var content = await response.Content.ReadAsStringAsync();
        var actualRooms = JsonSerializer.Deserialize<List<ReservableRoomDto>>(content, _jsonOptions);

        Assert.NotNull(actualRooms);
        Assert.Single(actualRooms);
        Assert.Equal(expectedRooms[0].ReservableRoomId, actualRooms[0].ReservableRoomId);
        Assert.True(actualRooms[0].IsAvailable);
    }

    [Fact]
    public async Task GetAvailableRooms_NoFilters_ShouldReturnAllAvailableRooms()
    {
        // Arrange
        var expectedRooms = new List<ReservableRoomDto>
        {
            new ReservableRoomDto
            {
                ReservableRoomId = "reservable-001",
                RoomId = "room-001",
                RoomName = "会議室A",
                Capacity = 10,
                IsAvailable = true,
                Date = DateTime.Today
            },
            new ReservableRoomDto
            {
                ReservableRoomId = "reservable-002",
                RoomId = "room-002",
                RoomName = "会議室B", 
                Capacity = 20,
                IsAvailable = true,
                Date = DateTime.Today
            }
        };

        _mockRoomService
            .Setup(x => x.GetAvailableRoomsAsync(It.IsAny<GetRoomsRequestDto>(), CancellationToken.None))
            .ReturnsAsync(expectedRooms);

        // Act
        var response = await _client.GetAsync("/api/rooms/available");

        // Assert
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        
        var content = await response.Content.ReadAsStringAsync();
        var actualRooms = JsonSerializer.Deserialize<List<ReservableRoomDto>>(content, _jsonOptions);

        Assert.NotNull(actualRooms);
        Assert.Equal(2, actualRooms.Count);
        Assert.All(actualRooms, room => Assert.True(room.IsAvailable));
    }
}
