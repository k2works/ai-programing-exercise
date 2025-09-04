using System.Data;
using System.Text.Json;
using Dapper;
using MRS.Application.Ports;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;
using MRS.Infrastructure.Data;

namespace MRS.Infrastructure.Repositories;

public class ReservationRepository : IReservationRepository
{
    private readonly IDbConnectionFactory _connectionFactory;

    public ReservationRepository(IDbConnectionFactory connectionFactory)
    {
        _connectionFactory = connectionFactory ?? throw new ArgumentNullException(nameof(connectionFactory));
    }

    public async Task<Reservation?> GetByIdAsync(string reservationId)
    {
        using var connection = _connectionFactory.CreateConnection();
        
        const string sql = @"
            SELECT ReservationId, RoomId, UserId, Title, StartTime, EndTime, 
                   Participants, Status, RowVersion, CreatedAt, UpdatedAt
            FROM Reservations 
            WHERE ReservationId = @ReservationId";

        var result = await connection.QuerySingleOrDefaultAsync(sql, new { ReservationId = reservationId });
        
        return result != null ? MapToReservation(result) : null;
    }

    public async Task<IReadOnlyList<Reservation>> GetAllAsync()
    {
        using var connection = _connectionFactory.CreateConnection();
        
        const string sql = @"
            SELECT ReservationId, RoomId, UserId, Title, StartTime, EndTime, 
                   Participants, Status, RowVersion, CreatedAt, UpdatedAt
            FROM Reservations 
            ORDER BY StartTime";

        var results = await connection.QueryAsync(sql);
        
        return results.Select(MapToReservation).ToList().AsReadOnly();
    }

    public async Task<IReadOnlyList<Reservation>> GetByRoomIdAsync(string roomId)
    {
        using var connection = _connectionFactory.CreateConnection();
        
        const string sql = @"
            SELECT ReservationId, RoomId, UserId, Title, StartTime, EndTime, 
                   Participants, Status, RowVersion, CreatedAt, UpdatedAt
            FROM Reservations 
            WHERE RoomId = @RoomId
            ORDER BY StartTime";

        var results = await connection.QueryAsync(sql, new { RoomId = roomId });
        
        return results.Select(MapToReservation).ToList().AsReadOnly();
    }

    public async Task<IReadOnlyList<Reservation>> GetByUserIdAsync(string userId)
    {
        using var connection = _connectionFactory.CreateConnection();
        
        const string sql = @"
            SELECT ReservationId, RoomId, UserId, Title, StartTime, EndTime, 
                   Participants, Status, RowVersion, CreatedAt, UpdatedAt
            FROM Reservations 
            WHERE UserId = @UserId
            ORDER BY StartTime";

        var results = await connection.QueryAsync(sql, new { UserId = userId });
        
        return results.Select(MapToReservation).ToList().AsReadOnly();
    }

    public async Task<IReadOnlyList<Reservation>> GetByDateRangeAsync(DateTime startDate, DateTime endDate)
    {
        using var connection = _connectionFactory.CreateConnection();
        
        const string sql = @"
            SELECT ReservationId, RoomId, UserId, Title, StartTime, EndTime, 
                   Participants, Status, RowVersion, CreatedAt, UpdatedAt
            FROM Reservations 
            WHERE StartTime <= @EndDate AND EndTime >= @StartDate
            ORDER BY StartTime";

        var results = await connection.QueryAsync(sql, new { StartDate = startDate, EndDate = endDate });
        
        return results.Select(MapToReservation).ToList().AsReadOnly();
    }

    public async Task<bool> HasConflictingReservationAsync(string roomId, DateTime startTime, DateTime endTime, string? excludeReservationId = null)
    {
        using var connection = _connectionFactory.CreateConnection();
        
        var sql = @"
            SELECT COUNT(*) 
            FROM Reservations 
            WHERE RoomId = @RoomId 
              AND Status = 'confirmed'
              AND StartTime < @EndTime 
              AND EndTime > @StartTime";
        
        object parameters;
        
        if (!string.IsNullOrEmpty(excludeReservationId))
        {
            sql += " AND ReservationId != @ExcludeReservationId";
            parameters = new { 
                RoomId = roomId, 
                StartTime = startTime, 
                EndTime = endTime, 
                ExcludeReservationId = excludeReservationId 
            };
        }
        else
        {
            parameters = new { RoomId = roomId, StartTime = startTime, EndTime = endTime };
        }

        var count = await connection.QuerySingleAsync<int>(sql, parameters);
        return count > 0;
    }

    public async Task<string> CreateAsync(Reservation reservation)
    {
        using var connection = _connectionFactory.CreateConnection();
        
        var reservationId = Guid.NewGuid().ToString();
        
        const string sql = @"
            INSERT INTO Reservations (ReservationId, RoomId, UserId, Title, StartTime, EndTime, 
                                    Participants, Status, RowVersion, CreatedAt, UpdatedAt)
            VALUES (@ReservationId, @RoomId, @UserId, @Title, @StartTime, @EndTime, 
                    @Participants, @Status, @RowVersion, @CreatedAt, @UpdatedAt)";

        var participantsJson = JsonSerializer.Serialize(reservation.Participants);
        
        await connection.ExecuteAsync(sql, new
        {
            ReservationId = reservationId,
            reservation.RoomId,
            reservation.UserId,
            reservation.Title,
            StartTime = reservation.TimeSlot.StartTime,
            EndTime = reservation.TimeSlot.EndTime,
            Participants = participantsJson,
            reservation.Status,
            reservation.RowVersion,
            reservation.CreatedAt,
            reservation.UpdatedAt
        });

        return reservationId;
    }

    public async Task<bool> UpdateAsync(Reservation reservation)
    {
        using var connection = _connectionFactory.CreateConnection();
        
        const string sql = @"
            UPDATE Reservations 
            SET Title = @Title,
                StartTime = @StartTime,
                EndTime = @EndTime,
                Participants = @Participants,
                Status = @Status,
                RowVersion = @NewRowVersion,
                UpdatedAt = @UpdatedAt
            WHERE ReservationId = @ReservationId 
              AND RowVersion = @CurrentRowVersion";

        var participantsJson = JsonSerializer.Serialize(reservation.Participants);
        
        var rowsAffected = await connection.ExecuteAsync(sql, new
        {
            reservation.ReservationId,
            reservation.Title,
            StartTime = reservation.TimeSlot.StartTime,
            EndTime = reservation.TimeSlot.EndTime,
            Participants = participantsJson,
            reservation.Status,
            CurrentRowVersion = reservation.RowVersion,
            NewRowVersion = reservation.RowVersion + 1,
            UpdatedAt = DateTime.UtcNow
        });

        if (rowsAffected > 0)
        {
            reservation.UpdateRowVersion(reservation.RowVersion + 1);
            return true;
        }

        return false;
    }

    public async Task<bool> DeleteAsync(string reservationId)
    {
        using var connection = _connectionFactory.CreateConnection();
        
        const string sql = "DELETE FROM Reservations WHERE ReservationId = @ReservationId";
        
        var rowsAffected = await connection.ExecuteAsync(sql, new { ReservationId = reservationId });
        
        return rowsAffected > 0;
    }

    private static Reservation MapToReservation(dynamic row)
    {
        var participantsString = row.Participants?.ToString() ?? string.Empty;
        var participants = string.IsNullOrEmpty(participantsString) 
            ? new List<string>()
            : JsonSerializer.Deserialize<List<string>>(participantsString) ?? new List<string>();

        var timeSlot = new TimeSlot(DateTime.Parse(row.StartTime.ToString()), DateTime.Parse(row.EndTime.ToString()));
        
        return Reservation.Restore(
            row.ReservationId.ToString(),
            row.RoomId.ToString(),
            row.UserId.ToString(),
            row.Title.ToString(),
            timeSlot,
            participants,
            row.Status.ToString(),
            Convert.ToInt32(row.RowVersion),
            DateTime.Parse(row.CreatedAt.ToString()),
            DateTime.Parse(row.UpdatedAt.ToString())
        );
    }
}