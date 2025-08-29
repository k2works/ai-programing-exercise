package mrs.infrastructure.in.web;

import java.time.LocalDate;
import java.util.List;
import mrs.application.domain.model.MeetingRoom;
import mrs.application.domain.model.ReservableRoom;
import mrs.infrastructure.out.db.RoomMapper;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/rooms")
public class RoomController {
    private final RoomMapper roomMapper;

    public RoomController(RoomMapper roomMapper) {
        this.roomMapper = roomMapper;
    }

    @GetMapping
    public ResponseEntity<List<MeetingRoom>> listRooms() {
        return ResponseEntity.ok(roomMapper.findAllRooms());
    }

    @GetMapping("/{date}")
    public ResponseEntity<List<ReservableRoom>> listReservableByDate(
        @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date
    ) {
        return ResponseEntity.ok(roomMapper.findReservableByDate(date));
    }
}
