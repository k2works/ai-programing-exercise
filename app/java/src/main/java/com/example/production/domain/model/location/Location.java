package com.example.production.domain.model.location;

import lombok.*;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Location {
    private String locationCode;
    private String locationName;
    private LocationType locationType;
    private String parentLocationCode;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
