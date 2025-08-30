package mrs.e2e;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import mrs.config.TestBeansConfig;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Import(TestBeansConfig.class)
public class ApiSmokeTest {

    @Autowired
    MockMvc mockMvc;

    @Autowired
    ObjectMapper objectMapper;

    @Test
    void login_and_get_rooms() throws Exception {
        // login
        MvcResult auth = mockMvc.perform(post("/api/auth/login")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"username\":\"user1\",\"password\":\"demo\"}"))
            .andExpect(status().isOk())
            .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
            .andReturn();

        JsonNode node = objectMapper.readTree(auth.getResponse().getContentAsString());
        String token = node.get("accessToken").asText();

        // rooms
        mockMvc.perform(get("/api/rooms").header("Authorization", "Bearer " + token))
            .andExpect(status().isOk())
            .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON));

        // refresh
        MvcResult refreshed = mockMvc.perform(post("/api/auth/refresh").header("Authorization", "Bearer " + token))
            .andExpect(status().isOk())
            .andReturn();
        String newToken = objectMapper.readTree(refreshed.getResponse().getContentAsString()).get("accessToken").asText();
        mockMvc.perform(get("/api/rooms").header("Authorization", "Bearer " + newToken))
            .andExpect(status().isOk());
    }
}
