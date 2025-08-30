package mrs.e2e.cucumber.steps;

import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import io.cucumber.spring.CucumberContextConfiguration;
import mrs.config.TestBeansConfig;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.context.annotation.Import;
import org.springframework.http.ResponseEntity;

import static org.assertj.core.api.Assertions.assertThat;

@CucumberContextConfiguration
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@ActiveProfiles("test")
@Import(TestBeansConfig.class)
public class RoomSteps {

    @Autowired
    TestRestTemplate restTemplate;

    private ResponseEntity<String> lastResponse;

    @When("I request GET {string}")
    public void i_request_get(String endpoint) throws Exception {
        System.out.println("TestRestTemplate: " + restTemplate.getRootUri());
        System.out.println("Requesting: " + endpoint);
        
        lastResponse = restTemplate.getForEntity(endpoint, String.class);
        
        System.out.println("Response status: " + lastResponse.getStatusCode());
        System.out.println("Response body: " + lastResponse.getBody());
    }

    @Then("the response status should be {int}")
    public void the_response_status_should_be(Integer status) throws Exception {
        assertThat(lastResponse.getStatusCode().value()).isEqualTo(status);
    }

    @Then("the response should contain {string}")
    public void the_response_should_contain(String text) throws Exception {
        assertThat(lastResponse.getBody()).contains(text);
    }
}
