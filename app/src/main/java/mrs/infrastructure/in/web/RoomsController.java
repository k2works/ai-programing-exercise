package mrs.infrastructure.in.web;

import mrs.application.domain.model.reservation.ReservableRoomList;
import mrs.application.domain.model.reservation.ReservedDate;
import mrs.application.port.in.RoomUseCase;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

import java.time.LocalDate;

@Controller
public class RoomsController {
    private final RoomUseCase roomUseCase;

    public RoomsController(RoomUseCase roomUseCase) {
        this.roomUseCase = roomUseCase;
    }

    @GetMapping("/rooms")
    public String listRooms(Model model) {
        LocalDate today = LocalDate.now();
        return listRoomsByDate(today, model);
    }

    @GetMapping("/rooms/{date}")
    public String listRoomsByDate(
        @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date,
        Model model
    ) {
        ReservedDate reservedDate = new ReservedDate(date);
        ReservableRoomList rooms = roomUseCase.findReservableRooms(reservedDate);
        
        model.addAttribute("rooms", rooms.rooms());
        model.addAttribute("date", date);
        
        return "listRooms";
    }
}
