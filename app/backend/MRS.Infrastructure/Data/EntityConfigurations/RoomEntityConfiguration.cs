using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using MRS.Domain.Entities;

namespace MRS.Infrastructure.Data.EntityConfigurations;

/// <summary>
/// 会議室エンティティの設定
/// </summary>
public class RoomEntityConfiguration : IEntityTypeConfiguration<Room>
{
    public void Configure(EntityTypeBuilder<Room> builder)
    {
        // テーブル名
        builder.ToTable("Rooms");

        // 主キー
        builder.HasKey(x => x.RoomId);

        // プロパティ設定
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

        builder.Property(x => x.Capacity)
            .IsRequired();

        builder.Property(x => x.IsActive)
            .IsRequired();

        builder.Property(x => x.CreatedAt)
            .IsRequired();

        builder.Property(x => x.UpdatedAt)
            .IsRequired();
    }
}
