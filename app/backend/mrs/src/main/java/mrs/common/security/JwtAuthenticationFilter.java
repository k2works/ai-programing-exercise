package mrs.common.security;

import io.jsonwebtoken.Claims;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.filter.OncePerRequestFilter;

public class JwtAuthenticationFilter extends OncePerRequestFilter {
    private final JwtService jwtService;
    private static final Logger log = LoggerFactory.getLogger(JwtAuthenticationFilter.class);

    public JwtAuthenticationFilter(JwtService jwtService) {
        this.jwtService = jwtService;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
        throws ServletException, IOException {
        String header = request.getHeader("Authorization");
        if (header != null && header.startsWith("Bearer ")) {
            String token = header.substring(7);
            try {
                Claims claims = jwtService.parseAndValidate(token);
                String subject = claims.getSubject();
                Object roles = claims.get("roles");
                Collection<? extends GrantedAuthority> authorities = toAuthorities(roles);
                
                // Create UserDetails object
                UserDetails userDetails = User.builder()
                    .username(subject)
                    .password("") // JWT authentication doesn't need password
                    .authorities(authorities)
                    .build();
                
                // Create authentication token with UserDetails as principal
                UsernamePasswordAuthenticationToken auth = 
                    new UsernamePasswordAuthenticationToken(userDetails, token, authorities);
                
                SecurityContextHolder.getContext().setAuthentication(auth);
            } catch (Exception ex) {
                // invalid token -> leave unauthenticated, but record at debug level
                if (log.isDebugEnabled()) {
                    log.debug("JWT validation failed: {}", ex.getMessage());
                }
            }
        }
        chain.doFilter(request, response);
    }

    private Collection<? extends GrantedAuthority> toAuthorities(Object roles) {
        if (roles instanceof String r) {
            return List.of(new SimpleGrantedAuthority("ROLE_" + r));
        }
        if (roles instanceof List<?> list && !list.isEmpty()) {
            return list.stream().map(Object::toString)
                .map(r -> new SimpleGrantedAuthority("ROLE_" + r))
                .toList();
        }
        return List.of();
    }
}
