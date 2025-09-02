package mrs.infrastructure.in.web;

import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

import jakarta.validation.Valid;
import mrs.application.dto.ReservationDto;
import mrs.application.dto.ReservationRequest;
import mrs.application.mapper.DtoMapper;
import mrs.application.port.in.ReservationUseCase;
import mrs.application.port.out.UserPort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
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
@RequestMapping("/api/reservations")
@Tag(name = "Reservations", description = "予約管理API")
@SecurityRequirement(name = "bearerAuth")
public class ReservationController {
    private static final String MEDIA_TYPE_JSON = "application/json";
    private final ReservationUseCase reservationUseCase;
    private final UserPort userPort;
    private final DtoMapper dtoMapper;

    public ReservationController(ReservationUseCase reservationUseCase, UserPort userPort, DtoMapper dtoMapper) {
        this.reservationUseCase = reservationUseCase;
        this.userPort = userPort;
        this.dtoMapper = dtoMapper;
    }

    @PostMapping
    @Operation(
        summary = "予約作成",
        description = "指定された条件で会議室を予約する"
    )
    @ApiResponses({
        @ApiResponse(
            responseCode = "201",
            description = "予約が正常に作成されました",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(
                    value = "{\"reservationId\": 1, \"startTime\": \"09:00\", \"endTime\": \"12:00\", " +
                           "\"reservableRoom\": {\"roomId\": 1, \"reservableDate\": \"2025-09-01\"}, " +
                           "\"userName\": \"山田太郎\", \"userId\": \"yamada\"}"
                )
            )
        ),
        @ApiResponse(
            responseCode = "400",
            description = "リクエストデータが不正",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"message\": \"Invalid request data\"}")
            )
        ),
        @ApiResponse(
            responseCode = "401",
            description = "認証が必要",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"message\": \"Unauthorized\"}")
            )
        ),
        @ApiResponse(
            responseCode = "409",
            description = "予約が重複している",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"message\": \"入力の時間帯はすでに予約済みです。\"}")
            )
        )
    })
    public ResponseEntity<ReservationDto> createReservation(
        @Valid @RequestBody ReservationRequest request,
        @AuthenticationPrincipal UserDetails userDetails
    ) {
        var user = userPort.findByUserId(userDetails.getUsername());
        var reservation = dtoMapper.toReservationDomain(request);
        var createdReservation = reservationUseCase.reserve(reservation, user);
        var responseDto = dtoMapper.toReservationDto(createdReservation);
        
        return ResponseEntity.status(HttpStatus.CREATED).body(responseDto);
    }

    @GetMapping("/{date}")
    @Operation(
        summary = "指定日の予約状況取得",
        description = "指定した日付の予約状況を取得する（全ユーザーまたは特定ユーザー）"
    )
    @ApiResponses({
        @ApiResponse(
            responseCode = "200",
            description = "指定日の予約状況一覧",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(
                    value = "[{\"reservationId\": 1, \"startTime\": \"09:00\", \"endTime\": \"12:00\", " +
                           "\"reservableRoom\": {\"roomId\": 1, \"reservableDate\": \"2025-09-01\"}, " +
                           "\"userName\": \"山田太郎\", \"userId\": \"yamada\"}]"
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
    public ResponseEntity<List<ReservationDto>> listReservationsByDate(
        @Parameter(
            description = "予約対象日（YYYY-MM-DD形式）",
            example = "2025-09-01"
        )
        @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date,
        @Parameter(
            description = "会議室ID（指定すれば特定の会議室の予約のみ取得）",
            example = "1"
        )
        @RequestParam(required = false) Integer roomId
    ) {
        var reservations = reservationUseCase.findReservations(roomId, date);
        var reservationDtos = reservations.stream()
            .map(dtoMapper::toReservationDto)
            .collect(Collectors.toList());
        return ResponseEntity.ok(reservationDtos);
    }

    @DeleteMapping("/{id}")
    @Operation(
        summary = "予約キャンセル",
        description = "指定された予約をキャンセルする（本人または管理者のみ可能）"
    )
    @ApiResponses({
        @ApiResponse(
            responseCode = "204",
            description = "予約が正常にキャンセルされました"
        ),
        @ApiResponse(
            responseCode = "403",
            description = "権限不足（他人の予約をキャンセルしようとした）",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"message\": \"Forbidden\"}")
            )
        ),
        @ApiResponse(
            responseCode = "404",
            description = "予約が見つかりません",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"message\": \"予約が見つかりません。\"}")
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
    public ResponseEntity<Void> cancelReservation(
        @Parameter(
            description = "予約ID",
            example = "1"
        )
        @PathVariable Integer id,
        @AuthenticationPrincipal UserDetails userDetails
    ) {
        var user = userPort.findByUserId(userDetails.getUsername());
        reservationUseCase.cancel(id, user);
        return ResponseEntity.noContent().build();
    }
}