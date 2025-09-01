package mrs.application.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Future;
import jakarta.validation.constraints.Min;
import mrs.application.validation.ValidTimeRange;
import java.time.LocalDate;
import java.time.LocalTime;

@Schema(description = "予約作成リクエスト")
@ValidTimeRange
public class ReservationRequest {
    @NotNull(message = "会議室IDは必須です")
    @Min(value = 1, message = "会議室IDは1以上である必要があります")
    @Schema(description = "会議室ID", example = "1")
    private Integer roomId;

    @NotNull(message = "予約日は必須です")
    @Future(message = "予約日は今日以降の日付である必要があります")
    @JsonFormat(pattern = "yyyy-MM-dd")
    @Schema(description = "予約日", example = "2025-09-01")
    private LocalDate reservableDate;

    @NotNull(message = "開始時間は必須です")
    @JsonFormat(pattern = "HH:mm")
    @Schema(description = "開始時間", example = "09:00")
    private LocalTime startTime;

    @NotNull(message = "終了時間は必須です")
    @JsonFormat(pattern = "HH:mm")
    @Schema(description = "終了時間", example = "12:00")
    private LocalTime endTime;

    public Integer getRoomId() { return roomId; }
    public void setRoomId(Integer roomId) { this.roomId = roomId; }

    public LocalDate getReservableDate() { return reservableDate; }
    public void setReservableDate(LocalDate reservableDate) { this.reservableDate = reservableDate; }

    public LocalTime getStartTime() { return startTime; }
    public void setStartTime(LocalTime startTime) { this.startTime = startTime; }

    public LocalTime getEndTime() { return endTime; }
    public void setEndTime(LocalTime endTime) { this.endTime = endTime; }
}