package mrs.infrastructure.in.web;

import mrs.application.domain.model.reservation.*;
import mrs.application.domain.model.room.*;
import mrs.application.port.in.RoomUseCase;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDate;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(controllers = RoomsController.class)
class RoomsControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private RoomUseCase roomUseCase;

    @Test
    @WithMockUser
    void 会議室一覧のモデルが正しく設定される() throws Exception {
        ReservableRoom room = createReservableRoom(1);
        ReservableRoomList roomList = new ReservableRoomList(List.of(room));
        
        when(roomUseCase.findReservableRooms(any(ReservedDate.class))).thenReturn(roomList);

        mockMvc.perform(get("/rooms"))
            .andExpect(model().attributeExists("rooms"))
            .andExpect(model().attributeExists("date"));
    }

    @Test
    @WithMockUser
    void 指定日の会議室一覧のモデルが正しく設定される() throws Exception {
        LocalDate date = LocalDate.of(2024, 1, 15);
        ReservableRoom room = createReservableRoom(1);
        ReservableRoomList roomList = new ReservableRoomList(List.of(room));
        
        when(roomUseCase.findReservableRooms(any(ReservedDate.class))).thenReturn(roomList);

        mockMvc.perform(get("/rooms/{date}", date))
            .andExpect(model().attribute("date", date));
    }

    private ReservableRoom createReservableRoom(int roomId) {
        ReservableRoomId id = new ReservableRoomId(LocalDate.of(2024, 1, 15), new RoomId(roomId));
        MeetingRoom meetingRoom = new MeetingRoom(new RoomId(roomId), new RoomName("会議室" + roomId));
        return new ReservableRoom(id, meetingRoom);
    }
}
