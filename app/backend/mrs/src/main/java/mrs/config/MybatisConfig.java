package mrs.config;

import org.apache.ibatis.type.LocalDateTypeHandler;
import org.apache.ibatis.type.LocalTimeTypeHandler;
import org.mybatis.spring.annotation.MapperScan;
import org.mybatis.spring.boot.autoconfigure.ConfigurationCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.LocalDate;
import java.time.LocalTime;

@Configuration
@MapperScan("mrs.infrastructure.out.persistence")
public class MybatisConfig {
    
    @Bean
    public ConfigurationCustomizer mybatisConfigurationCustomizer() {
        return configuration -> {
            configuration.getTypeHandlerRegistry().register(LocalDate.class, LocalDateTypeHandler.class);
            configuration.getTypeHandlerRegistry().register(LocalTime.class, LocalTimeTypeHandler.class);
        };
    }
}