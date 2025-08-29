using Microsoft.EntityFrameworkCore;
using MRS.Domain.Entities;
using MRS.Infrastructure.Data.EntityConfigurations;

namespace MRS.Infrastructure.Data;

/// <summary>
/// アプリケーションデータベースコンテキスト
/// </summary>
public class ApplicationDbContext : DbContext
{
    public ApplicationDbContext(DbContextOptions<ApplicationDbContext> options) : base(options)
    {
    }

    /// <summary>
    /// ユーザー
    /// </summary>
    public DbSet<User> Users { get; set; } = null!;

    /// <summary>
    /// 会議室
    /// </summary>
    public DbSet<Room> Rooms { get; set; } = null!;

    /// <summary>
    /// 予約可能会議室
    /// </summary>
    public DbSet<ReservableRoom> ReservableRooms { get; set; } = null!;

    // TODO: 予約機能実装時に有効化
    // /// <summary>
    // /// 予約
    // /// </summary>
    // public DbSet<Reservation> Reservations { get; set; } = null!;

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        base.OnModelCreating(modelBuilder);

        // エンティティ設定の適用
        modelBuilder.ApplyConfiguration(new UserEntityConfiguration());
        modelBuilder.ApplyConfiguration(new RoomEntityConfiguration());
        modelBuilder.ApplyConfiguration(new ReservableRoomEntityConfiguration());
        // TODO: 予約機能実装時に有効化
        // modelBuilder.ApplyConfiguration(new ReservationEntityConfiguration());
    }
}
