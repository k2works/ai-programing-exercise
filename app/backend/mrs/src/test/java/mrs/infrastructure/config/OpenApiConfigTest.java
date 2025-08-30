package mrs.infrastructure.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import org.junit.jupiter.api.Test;
import org.springframework.test.context.ActiveProfiles;

import static org.junit.jupiter.api.Assertions.*;

@ActiveProfiles("dev")
class OpenApiConfigTest {

    @Test
    void testCustomOpenAPI_Bean作成() {
        // Arrange
        OpenApiConfig config = new OpenApiConfig();
        
        // Act
        OpenAPI openAPI = config.customOpenAPI();
        
        // Assert
        assertNotNull(openAPI);
        
        // Info情報の確認
        Info info = openAPI.getInfo();
        assertNotNull(info);
        assertEquals("会議室予約システム（MRS）API", info.getTitle());
        assertEquals("会議室の予約・管理を行うREST API", info.getDescription());
        assertEquals("1.0.0", info.getVersion());
    }
    
    @Test
    void testCustomOpenAPI_Security設定() {
        // Arrange
        OpenApiConfig config = new OpenApiConfig();
        
        // Act
        OpenAPI openAPI = config.customOpenAPI();
        
        // Assert
        assertNotNull(openAPI.getSecurity());
        assertFalse(openAPI.getSecurity().isEmpty());
        
        SecurityRequirement securityRequirement = openAPI.getSecurity().get(0);
        assertTrue(securityRequirement.containsKey("bearerAuth"));
    }
    
    @Test
    void testCustomOpenAPI_Components設定() {
        // Arrange
        OpenApiConfig config = new OpenApiConfig();
        
        // Act
        OpenAPI openAPI = config.customOpenAPI();
        
        // Assert
        assertNotNull(openAPI.getComponents());
        assertNotNull(openAPI.getComponents().getSecuritySchemes());
        assertTrue(openAPI.getComponents().getSecuritySchemes().containsKey("bearerAuth"));
        
        var bearerAuth = openAPI.getComponents().getSecuritySchemes().get("bearerAuth");
        assertEquals(io.swagger.v3.oas.models.security.SecurityScheme.Type.HTTP, bearerAuth.getType());
        assertEquals("bearer", bearerAuth.getScheme());
        assertEquals("JWT", bearerAuth.getBearerFormat());
        assertEquals("JWT認証トークン", bearerAuth.getDescription());
    }
}