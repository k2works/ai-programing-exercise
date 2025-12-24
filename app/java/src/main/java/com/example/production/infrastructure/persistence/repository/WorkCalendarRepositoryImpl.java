package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.WorkCalendarRepository;
import com.example.production.domain.model.calendar.WorkCalendar;
import com.example.production.infrastructure.persistence.mapper.WorkCalendarMapper;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public class WorkCalendarRepositoryImpl implements WorkCalendarRepository {

    private final WorkCalendarMapper workCalendarMapper;

    public WorkCalendarRepositoryImpl(WorkCalendarMapper workCalendarMapper) {
        this.workCalendarMapper = workCalendarMapper;
    }

    @Override
    public void save(WorkCalendar calendar) {
        workCalendarMapper.insert(calendar);
    }

    @Override
    public Optional<WorkCalendar> findByCodeAndDate(String calendarCode, LocalDate date) {
        return workCalendarMapper.findByCodeAndDate(calendarCode, date);
    }

    @Override
    public List<WorkCalendar> findByCalendarCode(String calendarCode) {
        return workCalendarMapper.findByCalendarCode(calendarCode);
    }

    @Override
    public List<WorkCalendar> findByCalendarCodeAndDateRange(String calendarCode, LocalDate fromDate, LocalDate toDate) {
        return workCalendarMapper.findByCalendarCodeAndDateRange(calendarCode, fromDate, toDate);
    }

    @Override
    public List<WorkCalendar> findWorkingDays(String calendarCode, LocalDate fromDate, LocalDate toDate) {
        return workCalendarMapper.findWorkingDays(calendarCode, fromDate, toDate);
    }

    @Override
    public void deleteAll() {
        workCalendarMapper.deleteAll();
    }
}
