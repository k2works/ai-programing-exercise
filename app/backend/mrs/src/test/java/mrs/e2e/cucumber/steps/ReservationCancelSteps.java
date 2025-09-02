package mrs.e2e.cucumber.steps;

import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.*;

import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

public class ReservationCancelSteps {

    @Autowired
    private CommonSteps commonSteps;

    private Integer reservationId;
    private ResponseEntity<String> lastResponse;

    @Given("ユーザー {string} が会議室 {int} を {string} の {string} から {string} まで予約済み")
    public void userHasReservation(String userId, int roomId, String date, String startTime, String endTime) {
        // 一時的にユーザーとしてログイン
        commonSteps.ユーザーとしてログインしている(userId);
        
        // 予約を作成
        Map<String, Object> requestBody = new HashMap<>();
        requestBody.put("roomId", roomId);
        requestBody.put("reservableDate", date);
        requestBody.put("startTime", startTime);
        requestBody.put("endTime", endTime);

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setBearerAuth(commonSteps.getAuthToken());
        HttpEntity<Map<String, Object>> entity = new HttpEntity<>(requestBody, headers);

        ResponseEntity<Map> response = commonSteps.getRestTemplate().postForEntity(
            "/api/reservations",
            entity,
            Map.class
        );

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.CREATED);
        reservationId = (Integer) response.getBody().get("reservationId");
        assertThat(reservationId).isNotNull();
    }

    @When("予約をキャンセルする")
    public void cancelReservation() {
        performDelete(reservationId, true);
    }

    @When("他人の予約をキャンセルしようとする")
    public void tryToCancelOthersReservation() {
        performDelete(reservationId, true);
    }

    @When("存在しない予約ID {int} をキャンセルしようとする")
    public void tryToCancelNonExistentReservation(int nonExistentId) {
        performDelete(nonExistentId, true);
    }

    @When("認証なしで予約をキャンセルしようとする")
    public void tryToCancelWithoutAuth() {
        performDelete(reservationId, false);
    }

    private void performDelete(Integer targetId, boolean withAuth) {
        HttpHeaders headers = new HttpHeaders();
        if (withAuth) {
            headers.setBearerAuth(commonSteps.getAuthToken());
        }
        HttpEntity<Void> entity = new HttpEntity<>(headers);

        lastResponse = commonSteps.getRestTemplate().exchange(
            "/api/reservations/" + targetId,
            HttpMethod.DELETE,
            entity,
            String.class
        );
        
        commonSteps.setLastResponse(lastResponse);
    }

    @Then("予約が削除されている")
    public void reservationIsDeleted() {
        // 予約一覧を取得して確認
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(commonSteps.getAuthToken());
        HttpEntity<Void> entity = new HttpEntity<>(headers);

        ResponseEntity<String> response = commonSteps.getRestTemplate().exchange(
            "/api/reservations/2025-09-03?roomId=1",
            HttpMethod.GET,
            entity,
            String.class
        );

        // 予約が存在しないことを確認（空配列または該当予約が含まれない）
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(response.getBody()).doesNotContain("\"reservationId\":" + reservationId);
    }

    @Then("予約がまだ存在している")
    public void reservationStillExists() {
        // 予約一覧を取得して確認
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(commonSteps.getAuthToken());
        HttpEntity<Void> entity = new HttpEntity<>(headers);

        ResponseEntity<String> response = commonSteps.getRestTemplate().exchange(
            "/api/reservations/2025-09-03?roomId=1",
            HttpMethod.GET,
            entity,
            String.class
        );

        // 予約が存在することを確認
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(response.getBody()).contains("14:00");
    }

    @Then("エラーメッセージ {string} が返される")
    public void errorMessageReturned(String expectedMessage) {
        assertThat(lastResponse.getBody()).contains(expectedMessage);
    }
}