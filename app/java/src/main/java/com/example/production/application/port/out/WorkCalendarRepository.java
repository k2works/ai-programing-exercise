package com.example.production.application.port.out;

import com.example.production.domain.model.calendar.WorkCalendar;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * カレンダリポジトリ（Output Port）
 */
public interface WorkCalendarRepository {

    void save(WorkCalendar calendar);

    Optional<WorkCalendar> findByCodeAndDate(String calendarCode, LocalDate date);

    List<WorkCalendar> findByCalendarCode(String calendarCode);

    List<WorkCalendar> findByCalendarCodeAndDateRange(String calendarCode, LocalDate fromDate, LocalDate toDate);

    List<WorkCalendar> findWorkingDays(String calendarCode, LocalDate fromDate, LocalDate toDate);

    void deleteAll();
}
