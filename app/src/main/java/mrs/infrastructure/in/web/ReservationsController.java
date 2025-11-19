package mrs.infrastructure.in.web;

import jakarta.validation.Valid;
import mrs.application.domain.model.auth.User;
import mrs.application.domain.model.reservation.*;
import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.room.RoomId;
import mrs.application.port.in.ReservationUseCase;
import mrs.application.port.in.RoomUseCase;
import mrs.application.service.auth.AuthUserDetails;
import mrs.application.service.reservation.AlreadyReservedException;
import mrs.application.service.reservation.ReservationService;
import mrs.application.service.reservation.UnavailableReservationException;
import mrs.infrastructure.in.web.form.ReservationForm;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;

@Controller
@RequestMapping("/reservations")
public class ReservationsController {
    private final ReservationUseCase reservationUseCase;
    private final RoomUseCase roomUseCase;
    private final ReservationService reservationService;

    public ReservationsController(
        ReservationUseCase reservationUseCase,
        RoomUseCase roomUseCase,
        ReservationService reservationService
    ) {
        this.reservationUseCase = reservationUseCase;
        this.roomUseCase = roomUseCase;
        this.reservationService = reservationService;
    }

    @GetMapping("/{date}/{roomId}")
    public String reserveForm(
        @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date,
        @PathVariable Integer roomId,
        Model model
    ) {
        ReservableRoomId reservableRoomId = new ReservableRoomId(date, new RoomId(roomId));
        ReservationList reservations = reservationUseCase.findReservations(reservableRoomId);
        MeetingRoom meetingRoom = roomUseCase.findMeetingRoom(new RoomId(roomId));

        model.addAttribute("reservationForm", new ReservationForm());
        model.addAttribute("reservations", reservations.reservations());
        model.addAttribute("meetingRoom", meetingRoom);
        model.addAttribute("date", date);
        model.addAttribute("roomId", roomId);

        return "reserveForm";
    }

    @PostMapping("/{date}/{roomId}")
    public String reserve(
        @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date,
        @PathVariable Integer roomId,
        @Valid @ModelAttribute ReservationForm form,
        BindingResult bindingResult,
        @AuthenticationPrincipal AuthUserDetails userDetails,
        Model model
    ) {
        if (bindingResult.hasErrors()) {
            return reserveForm(date, roomId, model);
        }

        ReservableRoomId reservableRoomId = new ReservableRoomId(date, new RoomId(roomId));
        Reservation reservation = new Reservation(
            null,
            new ReservationTimeSlot(form.getStartTime(), form.getEndTime()),
            reservableRoomId,
            userDetails.getUser().userId()
        );

        try {
            reservationUseCase.reserve(reservation);
            return "redirect:/reservations/" + date + "/" + roomId;
        } catch (UnavailableReservationException | AlreadyReservedException e) {
            model.addAttribute("error", e.getMessage());
            return reserveForm(date, roomId, model);
        }
    }

    @PostMapping("/{reservationId}")
    public String cancel(
        @PathVariable Integer reservationId,
        @AuthenticationPrincipal AuthUserDetails userDetails
    ) {
        Reservation reservation = reservationUseCase.findOne(new ReservationId(reservationId));
        User user = userDetails.getUser();

        if (!reservationService.canCancel(reservation, user)) {
            throw new SecurityException("Cannot cancel this reservation");
        }

        reservationUseCase.cancel(reservation);
        
        LocalDate date = reservation.reservableRoomId().reservedDate();
        Integer roomId = reservation.reservableRoomId().roomId().value();
        
        return "redirect:/reservations/" + date + "/" + roomId;
    }
}
