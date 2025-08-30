package mrs.infrastructure.out.security;

import io.jsonwebtoken.Claims;
import mrs.application.port.out.JwtPort;
import mrs.common.security.JwtService;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

/**
 * JWT機能のインフラストラクチャアダプター
 * JwtServiceをApplication層のJwtPortに適合させる
 */
@Component
public class JwtAdapter implements JwtPort {
    
    private final JwtService jwtService;

    public JwtAdapter(JwtService jwtService) {
        this.jwtService = jwtService;
    }

    @Override
    public String createAccessToken(String subject, Map<String, Object> claims) {
        return jwtService.createAccessToken(subject, claims);
    }

    @Override
    public Map<String, Object> parseAndValidate(String token) {
        Claims claims = jwtService.parseAndValidate(token);
        Map<String, Object> claimsMap = new HashMap<>(claims);
        claimsMap.put("subject", claims.getSubject());
        return claimsMap;
    }

    @Override
    public long getExpirationTime() {
        return jwtService.getExpirationTime();
    }
}