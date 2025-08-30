package mrs.infrastructure.in.web;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;
import mrs.application.domain.model.room.ReservableRoom;
import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.dto.MeetingRoomDto;
import mrs.application.dto.ReservableRoomDto;
import mrs.application.mapper.DtoMapper;
import mrs.application.port.in.RoomUseCase;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@RestController
@RequestMapping("/api/rooms")
@Tag(name = "Rooms", description = "会議室管理API")
@SecurityRequirement(name = "bearerAuth")
public class RoomController {
    private static final String MEDIA_TYPE_JSON = "application/json";
    private final RoomUseCase roomUseCase;
    private final DtoMapper dtoMapper;

    public RoomController(RoomUseCase roomUseCase, DtoMapper dtoMapper) {
        this.roomUseCase = roomUseCase;
        this.dtoMapper = dtoMapper;
    }

    @GetMapping
    @Operation(
        summary = "会議室一覧取得",
        description = "システム内のすべての会議室の基本情報を取得する"
    )
    @ApiResponses({
        @ApiResponse(
            responseCode = "200",
            description = "会議室一覧",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(
                    value = "[{\"roomId\": 1, \"roomName\": \"新木場\"}, {\"roomId\": 2, \"roomName\": \"辰巳\"}]"
                )
            )
        ),
        @ApiResponse(
            responseCode = "401",
            description = "認証が必要",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"message\": \"Unauthorized\"}")
            )
        )
    })
    public ResponseEntity<List<MeetingRoomDto>> listRooms() {
        List<MeetingRoom> rooms = roomUseCase.findAllMeetingRooms();
        List<MeetingRoomDto> roomDtos = rooms.stream()
            .map(dtoMapper::toMeetingRoomDto)
            .collect(Collectors.toList());
        return ResponseEntity.ok(roomDtos);
    }

    @GetMapping("/{date}")
    @Operation(
        summary = "指定日の予約可能会議室取得",
        description = "指定した日付における予約可能な会議室の一覧を取得する"
    )
    @ApiResponses({
        @ApiResponse(
            responseCode = "200",
            description = "指定日の予約可能会議室一覧",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(
                    value = "[{\"reservableRoomId\": {\"meetingRoom\": " +
                           "{\"roomId\": 1, \"roomName\": \"新木場\"}, \"reservedDate\": \"2025-08-30\"}}]"
                )
            )
        ),
        @ApiResponse(
            responseCode = "400",
            description = "日付形式不正",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"message\": \"Invalid date format\"}")
            )
        ),
        @ApiResponse(
            responseCode = "401",
            description = "認証が必要",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"message\": \"Unauthorized\"}")
            )
        )
    })
    public ResponseEntity<List<ReservableRoomDto>> listReservableByDate(
        @Parameter(
            description = "予約対象日（YYYY-MM-DD形式）",
            example = "2025-08-30"
        )
        @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date
    ) {
        List<ReservableRoom> reservableRooms = roomUseCase.findReservableRooms(date);
        List<ReservableRoomDto> reservableRoomDtos = reservableRooms.stream()
            .map(dtoMapper::toReservableRoomDto)
            .collect(Collectors.toList());
        return ResponseEntity.ok(reservableRoomDtos);
    }
}
