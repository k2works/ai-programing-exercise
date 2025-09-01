package mrs.application.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalTime;

@Schema(description = "予約情報")
public class ReservationDto {
    @Schema(description = "予約ID", example = "1")
    private Integer reservationId;

    @JsonFormat(pattern = "HH:mm")
    @Schema(description = "開始時間", example = "09:00")
    private LocalTime startTime;

    @JsonFormat(pattern = "HH:mm")
    @Schema(description = "終了時間", example = "12:00")
    private LocalTime endTime;

    @Schema(description = "予約可能会議室情報")
    private ReservableRoomDto reservableRoom;

    @Schema(description = "ユーザー名", example = "山田太郎")
    private String userName;

    @Schema(description = "ユーザーID", example = "yamada")
    private String userId;

    public Integer getReservationId() { return reservationId; }
    public void setReservationId(Integer reservationId) { this.reservationId = reservationId; }

    public LocalTime getStartTime() { return startTime; }
    public void setStartTime(LocalTime startTime) { this.startTime = startTime; }

    public LocalTime getEndTime() { return endTime; }
    public void setEndTime(LocalTime endTime) { this.endTime = endTime; }

    public ReservableRoomDto getReservableRoom() { 
        return reservableRoom;
    }
    
    public void setReservableRoom(ReservableRoomDto reservableRoom) { 
        this.reservableRoom = reservableRoom;
    }

    public String getUserName() { return userName; }
    public void setUserName(String userName) { this.userName = userName; }

    public String getUserId() { return userId; }
    public void setUserId(String userId) { this.userId = userId; }
}