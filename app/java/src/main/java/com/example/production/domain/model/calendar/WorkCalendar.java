package com.example.production.domain.model.calendar;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class WorkCalendar {
    private String calendarCode;
    private LocalDate date;
    private DateType dateType;
    private BigDecimal workingHours;
    private String note;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
