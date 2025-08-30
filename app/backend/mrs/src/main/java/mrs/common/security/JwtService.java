package mrs.common.security;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import javax.crypto.SecretKey;
import java.time.Duration;
import java.time.Instant;
import java.util.Date;
import java.util.Map;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class JwtService {
    @Value("${app.security.jwt.issuer}")
    String issuer;

    @Value("${app.security.jwt.access-token-ttl}")
    Duration accessTtl;

    // default: Base64 of 'change-me-change-me-change-me-change-me'
    @Value("${JWT_SECRET:Y2hhbmdlLW1lLWNoYW5nZS1tZS1jaGFuZ2UtbWUtY2hhbmdlLW1l}")
    String secret;

    private SecretKey key() {
        byte[] keyBytes = Decoders.BASE64.decode(secret);
        return Keys.hmacShaKeyFor(keyBytes);
    }

    public String createAccessToken(String subject, Map<String, Object> claims) {
        Instant now = Instant.now();
        Instant exp = now.plus(accessTtl);
        return Jwts.builder()
            .issuer(issuer)
            .subject(subject)
            .claims(claims)
            .issuedAt(Date.from(now))
            .expiration(Date.from(exp))
            .signWith(key(), SignatureAlgorithm.HS256)
            .compact();
    }

    public Claims parseAndValidate(String token) {
        return Jwts.parser()
            .requireIssuer(issuer)
            .verifyWith(key())
            .build()
            .parseSignedClaims(token)
            .getPayload();
    }
}
