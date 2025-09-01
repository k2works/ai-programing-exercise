package mrs;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import mrs.common.security.JwtAuthenticationFilter;
import mrs.common.security.JwtService;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

@Configuration
@EnableMethodSecurity
@Profile("!test")
public class WebSecurityConfig {

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http
            .csrf(csrf -> csrf.disable())
            .cors(cors -> cors.configurationSource(corsConfigurationSource()))
            .sessionManagement(sm -> sm.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
            .authorizeHttpRequests(auth -> auth
                .requestMatchers(
                    "/api/auth/login", 
                    "/api/auth/refresh", 
                    "/actuator/health",
                    "/swagger-ui/**",
                    "/swagger-ui.html",
                    "/v3/api-docs/**",
                    "/v3/api-docs.yaml",
                    "/swagger-resources/**",
                    "/webjars/**"
                ).permitAll()
                .anyRequest().authenticated()
            )
            .httpBasic(Customizer.withDefaults());

        // ★ルートアクセス時、swagger-ui.htmlへリダイレクト
        http.addFilterBefore((ServletRequest request, ServletResponse response, FilterChain chain) -> {
            HttpServletRequest req = (HttpServletRequest) request;
            HttpServletResponse res = (HttpServletResponse) response;
            if ("/".equals(req.getRequestURI())) {
                res.sendRedirect("/swagger-ui.html");
                return;
            }
            chain.doFilter(request, response);
        }, UsernamePasswordAuthenticationFilter.class);

        http.addFilterBefore(jwtAuthenticationFilter(), UsernamePasswordAuthenticationFilter.class);
        return http.build();
    }

    @Bean
    public BCryptPasswordEncoder passwordEncoder() { return new BCryptPasswordEncoder(); }

    @Bean
    public JwtAuthenticationFilter jwtAuthenticationFilter() { return new JwtAuthenticationFilter(jwtService()); }

    @Bean
    public JwtService jwtService() { return new JwtService(); }

    @Bean
    public CorsConfigurationSource corsConfigurationSource() {
        CorsConfiguration configuration = new CorsConfiguration();
        
        // フロントエンドのオリジンを許可
        configuration.addAllowedOrigin("http://localhost:3000");
        configuration.addAllowedOrigin("http://localhost:3001");
        configuration.addAllowedOrigin("http://localhost:3002");
        
        // すべてのHTTPメソッドを許可
        configuration.addAllowedMethod("*");
        
        // すべてのヘッダーを許可
        configuration.addAllowedHeader("*");
        
        // 認証情報（クッキー、認証ヘッダー等）を含むリクエストを許可
        configuration.setAllowCredentials(true);
        
        // プリフライトリクエストのキャッシュ時間（秒）
        configuration.setMaxAge(3600L);

        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/api/**", configuration);
        return source;
    }
}
