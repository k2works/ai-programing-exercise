package mrs.e2e.cucumber.steps;

import io.cucumber.datatable.DataTable;
import io.cucumber.java.en.When;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.*;

import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

public class SimpleReservationSteps {

    @Autowired
    private TestRestTemplate restTemplate;

    @Autowired
    private CommonSteps commonSteps;
    
    private Integer lastReservationId;
    private ResponseEntity<String> lastResponse;

    @When("予約を作成する:")
    public void 予約を作成する(DataTable dataTable) {
        Map<String, String> data = dataTable.asMap(String.class, String.class);
        
        Map<String, Object> requestBody = new HashMap<>();
        requestBody.put("roomId", Integer.parseInt(data.get("roomId")));
        requestBody.put("reservableDate", data.get("date"));
        requestBody.put("startTime", data.get("start"));
        requestBody.put("endTime", data.get("end"));

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setBearerAuth(commonSteps.getAuthToken());
        HttpEntity<Map<String, Object>> entity = new HttpEntity<>(requestBody, headers);

        ResponseEntity<Map> response = restTemplate.postForEntity(
            "/api/reservations",
            entity,
            Map.class
        );

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.CREATED);
        lastReservationId = (Integer) response.getBody().get("reservationId");
        assertThat(lastReservationId).isNotNull();
    }

    @When("作成した予約をキャンセルする")
    public void 作成した予約をキャンセルする() {
        HttpHeaders headers = new HttpHeaders();
        headers.setBearerAuth(commonSteps.getAuthToken());
        HttpEntity<Void> entity = new HttpEntity<>(headers);

        lastResponse = restTemplate.exchange(
            "/api/reservations/" + lastReservationId,
            HttpMethod.DELETE,
            entity,
            String.class
        );
        
        commonSteps.setLastResponse(lastResponse);
    }
}