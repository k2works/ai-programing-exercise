package mrs.e2e;

import io.cucumber.java.en.And;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import io.restassured.RestAssured;
import io.restassured.response.Response;
import org.springframework.boot.test.web.server.LocalServerPort;

import java.util.Map;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

public class ReservationCancelSteps {

    @LocalServerPort
    private int port;

    private Response response;
    private String authToken;
    private Integer reservationId;
    private static final String BASE_URL = "http://localhost:";
    private static final String AUTH_HEADER = "Authorization";
    private static final String BEARER_PREFIX = "Bearer ";
    private static final String RESERVATIONS_ENDPOINT = "/api/reservations/";

    @Given("ユーザー {string} が会議室 {int} を {string} の {string} から {string} まで予約済み")
    public void userHasReservation(String userId, int roomId, String date, String startTime, String endTime) {
        // ユーザーとしてログイン
        String loginToken = loginAsUser(userId);
        
        // 予約を作成
        String requestBody = String.format(
            "{\"roomId\": %d, \"reservableDate\": \"%s\", \"startTime\": \"%s\", \"endTime\": \"%s\"}",
            roomId, date, startTime, endTime);

        Response createResponse = given()
            .baseUri(BASE_URL + port)
            .header(AUTH_HEADER, BEARER_PREFIX + loginToken)
            .header("Content-Type", "application/json")
            .body(requestBody)
            .when()
            .post(RESERVATIONS_ENDPOINT.substring(0, RESERVATIONS_ENDPOINT.length() - 1))
            .then()
            .statusCode(201)
            .extract()
            .response();

        reservationId = createResponse.jsonPath().getInt("reservationId");
        assertNotNull(reservationId, "Reservation should be created with ID");
    }

    @When("予約をキャンセルする")
    public void cancelReservation() {
        response = given()
            .baseUri(BASE_URL + port)
            .header(AUTH_HEADER, BEARER_PREFIX + authToken)
            .when()
            .delete(RESERVATIONS_ENDPOINT + reservationId);
    }

    @When("他人の予約をキャンセルしようとする")
    public void tryToCancelOthersReservation() {
        response = given()
            .baseUri(BASE_URL + port)
            .header(AUTH_HEADER, BEARER_PREFIX + authToken)
            .when()
            .delete(RESERVATIONS_ENDPOINT + reservationId);
    }

    @When("存在しない予約ID {int} をキャンセルしようとする")
    public void tryToCancelNonExistentReservation(int nonExistentId) {
        response = given()
            .baseUri(BASE_URL + port)
            .header(AUTH_HEADER, BEARER_PREFIX + authToken)
            .when()
            .delete(RESERVATIONS_ENDPOINT + nonExistentId);
    }

    @When("認証なしで予約をキャンセルしようとする")
    public void tryToCancelWithoutAuth() {
        response = given()
            .baseUri(BASE_URL + port)
            .when()
            .delete(RESERVATIONS_ENDPOINT + reservationId);
    }

    @Then("予約が削除されている")
    public void reservationIsDeleted() {
        // 予約が存在しないことを確認（404が返ることを期待）
        given()
            .baseUri(BASE_URL + port)
            .header(AUTH_HEADER, BEARER_PREFIX + authToken)
            .when()
            .get(RESERVATIONS_ENDPOINT + reservationId)
            .then()
            .statusCode(404);
    }

    @Then("予約がまだ存在している")
    public void reservationStillExists() {
        // 予約の詳細を取得できることを確認
        Response checkResponse = given()
            .baseUri(BASE_URL + port)
            .header(AUTH_HEADER, BEARER_PREFIX + authToken)
            .when()
            .get(RESERVATIONS_ENDPOINT + "2025-09-03?roomId=1");
        
        // 予約リストに含まれていることを確認
        assertTrue(checkResponse.getBody().asString().contains("14:00"));
    }

    @Then("エラーメッセージ {string} が返される")
    public void errorMessageReturned(String expectedMessage) {
        response.then()
            .body("message", equalTo(expectedMessage));
    }

    @Given("ユーザー {string} としてログインしている")
    public void userIsLoggedIn(String userId) {
        authToken = loginAsUser(userId);
    }

    @Then("レスポンスステータスは {int} である")
    public void responseStatusIs(int expectedStatus) {
        response.then().statusCode(expectedStatus);
    }

    private String loginAsUser(String userId) {
        // パスワードを決定
        String password = switch (userId) {
            case "taro" -> "pass123";
            case "hanako" -> "pass456";
            case "admin" -> "admin123";
            default -> throw new IllegalArgumentException("Unknown user: " + userId);
        };

        // ログインリクエスト
        Response loginResponse = given()
            .baseUri(BASE_URL + port)
            .header("Content-Type", "application/json")
            .body(Map.of("userId", userId, "password", password))
            .when()
            .post("/api/auth/login");

        return loginResponse.jsonPath().getString("token");
    }
}