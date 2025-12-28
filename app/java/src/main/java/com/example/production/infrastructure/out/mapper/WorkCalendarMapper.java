package com.example.production.infrastructure.out.mapper;

import com.example.production.domain.model.calendar.WorkCalendar;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Mapper
public interface WorkCalendarMapper {
    void insert(WorkCalendar calendar);
    Optional<WorkCalendar> findByCodeAndDate(@Param("calendarCode") String calendarCode, @Param("date") LocalDate date);
    List<WorkCalendar> findByCalendarCode(String calendarCode);
    List<WorkCalendar> findByCalendarCodeAndDateRange(@Param("calendarCode") String calendarCode,
                                                       @Param("fromDate") LocalDate fromDate,
                                                       @Param("toDate") LocalDate toDate);
    List<WorkCalendar> findWorkingDays(@Param("calendarCode") String calendarCode,
                                        @Param("fromDate") LocalDate fromDate,
                                        @Param("toDate") LocalDate toDate);
    void deleteAll();
}
