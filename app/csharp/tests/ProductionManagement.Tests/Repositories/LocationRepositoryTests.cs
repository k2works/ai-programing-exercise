using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Location;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// 場所リポジトリテスト
/// </summary>
[Collection("Database")]
public class LocationRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly ILocationRepository _locationRepository;

    public LocationRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _locationRepository = new LocationRepository(fixture.ConnectionString);
        _locationRepository.DeleteAllAsync().Wait();
    }

    public class Registration : LocationRepositoryTests
    {
        public Registration(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 場所を登録できる()
        {
            // Arrange
            var location = new Location
            {
                LocationCode = "WH001",
                LocationName = "本社倉庫",
                LocationType = LocationType.Warehouse
            };

            // Act
            await _locationRepository.SaveAsync(location);

            // Assert
            var result = await _locationRepository.FindByCodeAsync("WH001");
            result.Should().NotBeNull();
            result!.LocationName.Should().Be("本社倉庫");
            result.LocationType.Should().Be(LocationType.Warehouse);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 全ての場所区分を登録できる()
        {
            var locationTypes = Enum.GetValues<LocationType>();
            var index = 0;

            foreach (var locationType in locationTypes)
            {
                var location = new Location
                {
                    LocationCode = $"LOC-{index:D3}",
                    LocationName = $"場所{locationType.GetDisplayName()}",
                    LocationType = locationType
                };

                await _locationRepository.SaveAsync(location);

                var result = await _locationRepository.FindByCodeAsync(location.LocationCode);
                result.Should().NotBeNull();
                result!.LocationType.Should().Be(locationType);

                index++;
            }
        }
    }

    public class Hierarchy : LocationRepositoryTests
    {
        public Hierarchy(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 親子関係を持つ場所を登録できる()
        {
            // Arrange: 親場所を登録
            var parentLocation = new Location
            {
                LocationCode = "WH001",
                LocationName = "本社倉庫",
                LocationType = LocationType.Warehouse
            };
            await _locationRepository.SaveAsync(parentLocation);

            // Act: 子場所を登録
            var childLocation = new Location
            {
                LocationCode = "WH001-A",
                LocationName = "本社倉庫A棟",
                LocationType = LocationType.Warehouse,
                ParentLocationCode = "WH001"
            };
            await _locationRepository.SaveAsync(childLocation);

            // Assert
            var result = await _locationRepository.FindByCodeAsync("WH001-A");
            result.Should().NotBeNull();
            result!.ParentLocationCode.Should().Be("WH001");
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 子場所を取得できる()
        {
            // Arrange: 親場所と複数の子場所を登録
            var parent = new Location
            {
                LocationCode = "FACTORY",
                LocationName = "本社工場",
                LocationType = LocationType.Manufacturing
            };
            await _locationRepository.SaveAsync(parent);

            var children = new[]
            {
                new Location { LocationCode = "FACTORY-L1", LocationName = "製造ライン1", LocationType = LocationType.Manufacturing, ParentLocationCode = "FACTORY" },
                new Location { LocationCode = "FACTORY-L2", LocationName = "製造ライン2", LocationType = LocationType.Manufacturing, ParentLocationCode = "FACTORY" },
                new Location { LocationCode = "FACTORY-INS", LocationName = "検査エリア", LocationType = LocationType.Inspection, ParentLocationCode = "FACTORY" }
            };

            foreach (var child in children)
            {
                await _locationRepository.SaveAsync(child);
            }

            // Act
            var result = await _locationRepository.FindChildrenAsync("FACTORY");

            // Assert
            result.Should().HaveCount(3);
            result.Select(l => l.LocationCode).Should().Contain(new[] { "FACTORY-L1", "FACTORY-L2", "FACTORY-INS" });
        }
    }

    public class TypeQuery : LocationRepositoryTests
    {
        public TypeQuery(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 場所区分で場所を検索できる()
        {
            // Arrange
            var locations = new[]
            {
                new Location { LocationCode = "WH001", LocationName = "倉庫1", LocationType = LocationType.Warehouse },
                new Location { LocationCode = "WH002", LocationName = "倉庫2", LocationType = LocationType.Warehouse },
                new Location { LocationCode = "MF001", LocationName = "製造1", LocationType = LocationType.Manufacturing },
                new Location { LocationCode = "SH001", LocationName = "出荷1", LocationType = LocationType.Shipping }
            };

            foreach (var location in locations)
            {
                await _locationRepository.SaveAsync(location);
            }

            // Act
            var warehouses = await _locationRepository.FindByTypeAsync(LocationType.Warehouse);

            // Assert
            warehouses.Should().HaveCount(2);
            warehouses.Should().OnlyContain(l => l.LocationType == LocationType.Warehouse);
        }
    }
}
