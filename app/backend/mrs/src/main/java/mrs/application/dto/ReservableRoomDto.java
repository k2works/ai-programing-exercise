package mrs.application.dto;

import java.time.LocalDate;

/**
 * 予約可能会議室DTO
 * Web層で予約可能会議室情報を表現するためのデータ転送オブジェクト
 * 不変クラスとして実装し、スレッドセーフ性を保証
 */
public final class ReservableRoomDto {
    private final Integer roomId;
    private final LocalDate reservableDate;

    public ReservableRoomDto() {
        this.roomId = null;
        this.reservableDate = null;
    }

    public ReservableRoomDto(Integer roomId, LocalDate reservableDate) {
        this.roomId = roomId;
        this.reservableDate = reservableDate;
    }

    /**
     * コピーコンストラクタ
     * 防御的コピーのために使用
     */
    public ReservableRoomDto(ReservableRoomDto other) {
        if (other != null) {
            this.roomId = other.roomId;
            this.reservableDate = other.reservableDate;
        } else {
            this.roomId = null;
            this.reservableDate = null;
        }
    }

    public Integer getRoomId() {
        return roomId;
    }

    public LocalDate getReservableDate() {
        return reservableDate;
    }
    
    /**
     * roomIdを変更した新しいインスタンスを作成
     */
    public ReservableRoomDto withRoomId(Integer roomId) {
        return new ReservableRoomDto(roomId, this.reservableDate);
    }
    
    /**
     * reservableDateを変更した新しいインスタンスを作成
     */
    public ReservableRoomDto withReservableDate(LocalDate reservableDate) {
        return new ReservableRoomDto(this.roomId, reservableDate);
    }
}