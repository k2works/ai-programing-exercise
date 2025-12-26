using FluentAssertions;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Calendar;
using ProductionManagement.Infrastructure.Persistence.Repositories;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests.Repositories;

/// <summary>
/// カレンダリポジトリテスト
/// </summary>
[Collection("Database")]
public class CalendarRepositoryTests
{
    private readonly PostgresFixture _fixture;
    private readonly ICalendarRepository _calendarRepository;

    public CalendarRepositoryTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _calendarRepository = new CalendarRepository(fixture.ConnectionString);
        _calendarRepository.DeleteAllAsync().Wait();
    }

    public class Registration : CalendarRepositoryTests
    {
        public Registration(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task カレンダを登録できる()
        {
            // Arrange
            var calendar = new WorkCalendar
            {
                CalendarCode = "MAIN",
                Date = new DateOnly(2025, 1, 6),
                DateType = DateType.Working,
                WorkingHours = 8.0m
            };

            // Act
            await _calendarRepository.SaveAsync(calendar);

            // Assert
            var result = await _calendarRepository.FindByCodeAndDateAsync("MAIN", new DateOnly(2025, 1, 6));
            result.Should().NotBeNull();
            result!.CalendarCode.Should().Be("MAIN");
            result.DateType.Should().Be(DateType.Working);
            result.WorkingHours.Should().Be(8.0m);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 全ての日付区分を登録できる()
        {
            var dateTypes = Enum.GetValues<DateType>();
            var baseDate = new DateOnly(2025, 1, 1);

            foreach (var dateType in dateTypes)
            {
                var calendar = new WorkCalendar
                {
                    CalendarCode = "TEST",
                    Date = baseDate,
                    DateType = dateType,
                    WorkingHours = dateType == DateType.HalfDay ? 4.0m : (dateType == DateType.Working ? 8.0m : null)
                };

                await _calendarRepository.SaveAsync(calendar);

                var result = await _calendarRepository.FindByCodeAndDateAsync("TEST", baseDate);
                result.Should().NotBeNull();
                result!.DateType.Should().Be(dateType);

                await _calendarRepository.DeleteAllAsync();
                baseDate = baseDate.AddDays(1);
            }
        }
    }

    public class DateRangeQuery : CalendarRepositoryTests
    {
        public DateRangeQuery(PostgresFixture fixture) : base(fixture) { }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 期間内のカレンダを取得できる()
        {
            // Arrange: 1週間分のカレンダを登録
            var startDate = new DateOnly(2025, 1, 6);
            for (int i = 0; i < 7; i++)
            {
                var date = startDate.AddDays(i);
                var isWeekend = date.DayOfWeek == DayOfWeek.Saturday || date.DayOfWeek == DayOfWeek.Sunday;
                var calendar = new WorkCalendar
                {
                    CalendarCode = "MAIN",
                    Date = date,
                    DateType = isWeekend ? DateType.Holiday : DateType.Working,
                    WorkingHours = isWeekend ? null : 8.0m
                };
                await _calendarRepository.SaveAsync(calendar);
            }

            // Act
            var result = await _calendarRepository.FindByCodeAndDateRangeAsync("MAIN", startDate, startDate.AddDays(6));

            // Assert
            result.Should().HaveCount(7);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 稼働日のみを取得できる()
        {
            // Arrange: 1週間分のカレンダを登録（平日5日、休日2日）
            var startDate = new DateOnly(2025, 1, 6); // 月曜日
            for (int i = 0; i < 7; i++)
            {
                var date = startDate.AddDays(i);
                var isWeekend = date.DayOfWeek == DayOfWeek.Saturday || date.DayOfWeek == DayOfWeek.Sunday;
                var calendar = new WorkCalendar
                {
                    CalendarCode = "MAIN",
                    Date = date,
                    DateType = isWeekend ? DateType.Holiday : DateType.Working,
                    WorkingHours = isWeekend ? null : 8.0m
                };
                await _calendarRepository.SaveAsync(calendar);
            }

            // Act
            var result = await _calendarRepository.FindWorkingDaysAsync("MAIN", startDate, startDate.AddDays(6));

            // Assert: 平日5日のみ
            result.Should().HaveCount(5);
            result.Should().OnlyContain(c => c.DateType == DateType.Working);
        }

        [Fact]
        [Trait("Category", "Integration")]
        public async Task 半日稼働も稼働日として取得できる()
        {
            // Arrange
            var calendars = new[]
            {
                new WorkCalendar { CalendarCode = "MAIN", Date = new DateOnly(2025, 1, 6), DateType = DateType.Working, WorkingHours = 8.0m },
                new WorkCalendar { CalendarCode = "MAIN", Date = new DateOnly(2025, 1, 7), DateType = DateType.HalfDay, WorkingHours = 4.0m },
                new WorkCalendar { CalendarCode = "MAIN", Date = new DateOnly(2025, 1, 8), DateType = DateType.Holiday }
            };

            foreach (var calendar in calendars)
            {
                await _calendarRepository.SaveAsync(calendar);
            }

            // Act
            var result = await _calendarRepository.FindWorkingDaysAsync("MAIN", new DateOnly(2025, 1, 6), new DateOnly(2025, 1, 8));

            // Assert: 稼働日と半日稼働の2日
            result.Should().HaveCount(2);
            result.Select(c => c.DateType).Should().Contain(new[] { DateType.Working, DateType.HalfDay });
        }
    }
}
