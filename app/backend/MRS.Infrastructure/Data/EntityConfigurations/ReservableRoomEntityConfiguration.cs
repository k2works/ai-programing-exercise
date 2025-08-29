using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using MRS.Domain.Entities;

namespace MRS.Infrastructure.Data.EntityConfigurations;

/// <summary>
/// 予約可能会議室エンティティの設定
/// </summary>
public class ReservableRoomEntityConfiguration : IEntityTypeConfiguration<ReservableRoom>
{
    public void Configure(EntityTypeBuilder<ReservableRoom> builder)
    {
        // テーブル名
        builder.ToTable("ReservableRooms");

        // 主キー
        builder.HasKey(x => x.ReservableRoomId);

        // プロパティ設定
        builder.Property(x => x.ReservableRoomId)
            .HasConversion(
                v => v.Value,
                v => new Domain.ValueObjects.ReservableRoomId(v))
            .HasMaxLength(50)
            .IsRequired();

        builder.Property(x => x.RoomId)
            .HasConversion(
                v => v.Value,
                v => new Domain.ValueObjects.RoomId(v))
            .HasMaxLength(50)
            .IsRequired();

        builder.Property(x => x.RoomName)
            .HasConversion(
                v => v.Value,
                v => new Domain.ValueObjects.Name(v))
            .HasMaxLength(100)
            .IsRequired();

        builder.Property(x => x.IsAvailable)
            .IsRequired();

        builder.Property(x => x.CreatedAt)
            .IsRequired();

        // 外部キー関係
        builder.HasOne<Room>()
            .WithMany()
            .HasForeignKey(x => x.RoomId)
            .OnDelete(DeleteBehavior.Restrict);

        // インデックス
        builder.HasIndex(x => x.RoomId);
    }
}
