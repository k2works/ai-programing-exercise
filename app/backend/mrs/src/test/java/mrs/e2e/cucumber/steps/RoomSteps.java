package mrs.e2e.cucumber.steps;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@CucumberContextConfiguration
@SpringBootTest
@AutoConfigureMockMvc
public class RoomSteps {

    @Autowired
    MockMvc mockMvc;

    @Autowired
    ObjectMapper objectMapper;

    private String token;
    private MvcResult lastResponse;

    @Given("I login with username {string} and password {string}")
    public void i_login_with_username_and_password(String username, String password) throws Exception {
        MvcResult auth = mockMvc.perform(post("/api/auth/login")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"username\":\"" + username + "\",\"password\":\"" + password + "\"}"))
            .andExpect(status().isOk())
            .andReturn();
        JsonNode node = objectMapper.readTree(auth.getResponse().getContentAsString());
        token = node.get("accessToken").asText();
        assertThat(token).isNotBlank();
    }

    @When("I request GET \\/api\\/rooms")
    public void i_request_get_rooms() throws Exception {
        lastResponse = mockMvc.perform(get("/api/rooms").header("Authorization", "Bearer " + token))
            .andReturn();
    }

    @Then("the response status should be {int}")
    public void the_response_status_should_be(Integer status) throws Exception {
        assertThat(lastResponse.getResponse().getStatus()).isEqualTo(status);
    }

    @Then("the response should contain {string}")
    public void the_response_should_contain(String text) throws Exception {
        assertThat(lastResponse.getResponse().getContentAsString()).contains(text);
    }
}
