package mrs.infrastructure.in.web;

import mrs.application.domain.model.auth.*;
import mrs.application.domain.model.reservation.*;
import mrs.application.domain.model.room.*;
import mrs.application.port.in.ReservationUseCase;
import mrs.application.port.in.RoomUseCase;
import mrs.application.service.auth.AuthUserDetails;
import mrs.application.service.reservation.ReservationService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.user;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(controllers = ReservationsController.class)
class ReservationsControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private ReservationService reservationService;

    @MockBean
    private RoomUseCase roomUseCase;

    @Test
    @WithMockUser
    void 予約フォームのモデルが正しく設定される() throws Exception {
        LocalDate date = LocalDate.of(2024, 1, 15);
        Integer roomId = 1;
        
        ReservationList reservations = new ReservationList(List.of());
        MeetingRoom meetingRoom = new MeetingRoom(new RoomId(roomId), new RoomName("会議室A"));
        
        when(reservationService.findReservations(any(ReservableRoomId.class))).thenReturn(reservations);
        when(roomUseCase.findMeetingRoom(any(RoomId.class))).thenReturn(meetingRoom);

        mockMvc.perform(get("/reservations/{date}/{roomId}", date, roomId))
            .andExpect(model().attributeExists("reservationForm"))
            .andExpect(model().attributeExists("reservations"))
            .andExpect(model().attributeExists("meetingRoom"));
    }

    @Test
    void 予約を作成できる() throws Exception {
        LocalDate date = LocalDate.of(2024, 1, 15);
        Integer roomId = 1;
        AuthUserDetails userDetails = createUserDetails();
        
        when(reservationService.reserve(any(Reservation.class))).thenReturn(null);

        mockMvc.perform(post("/reservations/{date}/{roomId}", date, roomId)
                .with(user(userDetails))
                .with(csrf())
                .param("startTime", "09:00")
                .param("endTime", "10:00"))
            .andExpect(status().is3xxRedirection())
            .andExpect(redirectedUrl("/reservations/" + date + "/" + roomId));
    }

    @Test
    void 予約をキャンセルできる() throws Exception {
        Integer reservationId = 1;
        LocalDate date = LocalDate.of(2024, 1, 15);
        Integer roomId = 1;
        
        Reservation reservation = createReservation(reservationId, date, roomId);
        AuthUserDetails userDetails = createUserDetails();
        
        when(reservationService.findOne(any(ReservationId.class))).thenReturn(reservation);
        when(reservationService.canCancel(any(Reservation.class), any(User.class))).thenReturn(true);

        mockMvc.perform(post("/reservations/{reservationId}", reservationId)
                .with(user(userDetails))
                .with(csrf()))
            .andExpect(status().is3xxRedirection())
            .andExpect(redirectedUrl("/reservations/" + date + "/" + roomId));

        verify(reservationService).cancel(reservation);
    }

    private AuthUserDetails createUserDetails() {
        User user = new User(
            new UserId("user001"),
            new Password("password"),
            new Name("山田", "太郎"),
            RoleName.USER
        );
        return new AuthUserDetails(user);
    }

    private Reservation createReservation(Integer id, LocalDate date, Integer roomId) {
        return new Reservation(
            new ReservationId(id),
            new ReservationTimeSlot(LocalTime.of(9, 0), LocalTime.of(10, 0)),
            new ReservableRoomId(date, new RoomId(roomId)),
            new UserId("user001")
        );
    }
}
