package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.WorkCalendarRepository;
import com.example.production.domain.model.calendar.DateType;
import com.example.production.domain.model.calendar.WorkCalendar;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("カレンダリポジトリ")
class WorkCalendarRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private WorkCalendarRepository workCalendarRepository;

    @BeforeEach
    void setUp() {
        workCalendarRepository.deleteAll();
    }

    @Nested
    @DisplayName("登録")
    class Registration {

        @Test
        @DisplayName("カレンダを登録できる")
        void canRegisterCalendar() {
            // Arrange
            var calendar = WorkCalendar.builder()
                    .calendarCode("CAL-001")
                    .date(LocalDate.of(2025, 1, 6))
                    .dateType(DateType.WORKING)
                    .workingHours(new BigDecimal("8.00"))
                    .build();

            // Act
            workCalendarRepository.save(calendar);

            // Assert
            var result = workCalendarRepository.findByCodeAndDate("CAL-001", LocalDate.of(2025, 1, 6));
            assertThat(result).isPresent();
            assertThat(result.get().getDateType()).isEqualTo(DateType.WORKING);
            assertThat(result.get().getWorkingHours()).isEqualByComparingTo(new BigDecimal("8.00"));
        }

        @Test
        @DisplayName("全ての日付区分を登録できる")
        void canRegisterAllDateTypes() {
            var types = DateType.values();

            for (int i = 0; i < types.length; i++) {
                var calendar = WorkCalendar.builder()
                        .calendarCode("CAL-001")
                        .date(LocalDate.of(2025, 1, 1 + i))
                        .dateType(types[i])
                        .build();

                workCalendarRepository.save(calendar);

                var result = workCalendarRepository.findByCodeAndDate("CAL-001", LocalDate.of(2025, 1, 1 + i));
                assertThat(result).isPresent();
                assertThat(result.get().getDateType()).isEqualTo(types[i]);
            }
        }
    }

    @Nested
    @DisplayName("検索")
    class Search {

        @Test
        @DisplayName("稼働日のみを検索できる")
        void canFindWorkingDays() {
            // Arrange: 稼働日と休日を登録
            workCalendarRepository.save(createCalendar("CAL-001", LocalDate.of(2025, 1, 6), DateType.WORKING));
            workCalendarRepository.save(createCalendar("CAL-001", LocalDate.of(2025, 1, 7), DateType.WORKING));
            workCalendarRepository.save(createCalendar("CAL-001", LocalDate.of(2025, 1, 8), DateType.HOLIDAY));
            workCalendarRepository.save(createCalendar("CAL-001", LocalDate.of(2025, 1, 9), DateType.WORKING));

            // Act
            var workingDays = workCalendarRepository.findWorkingDays(
                    "CAL-001",
                    LocalDate.of(2025, 1, 6),
                    LocalDate.of(2025, 1, 9)
            );

            // Assert
            assertThat(workingDays).hasSize(3);
            assertThat(workingDays).allMatch(c -> c.getDateType() == DateType.WORKING);
        }

        @Test
        @DisplayName("期間指定で検索できる")
        void canFindByDateRange() {
            // Arrange
            workCalendarRepository.save(createCalendar("CAL-001", LocalDate.of(2025, 1, 1), DateType.HOLIDAY));
            workCalendarRepository.save(createCalendar("CAL-001", LocalDate.of(2025, 1, 2), DateType.WORKING));
            workCalendarRepository.save(createCalendar("CAL-001", LocalDate.of(2025, 1, 3), DateType.WORKING));
            workCalendarRepository.save(createCalendar("CAL-001", LocalDate.of(2025, 1, 4), DateType.HALF_DAY));

            // Act
            var calendars = workCalendarRepository.findByCalendarCodeAndDateRange(
                    "CAL-001",
                    LocalDate.of(2025, 1, 2),
                    LocalDate.of(2025, 1, 3)
            );

            // Assert
            assertThat(calendars).hasSize(2);
        }
    }

    private WorkCalendar createCalendar(String code, LocalDate date, DateType type) {
        return WorkCalendar.builder()
                .calendarCode(code)
                .date(date)
                .dateType(type)
                .workingHours(type == DateType.WORKING ? new BigDecimal("8.00") :
                             type == DateType.HALF_DAY ? new BigDecimal("4.00") : null)
                .build();
    }
}
