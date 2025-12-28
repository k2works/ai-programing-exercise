package com.example.production.infrastructure.config;

import nz.net.ultraq.thymeleaf.layoutdialect.LayoutDialect;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.thymeleaf.spring6.SpringTemplateEngine;
import org.thymeleaf.spring6.templateresolver.SpringResourceTemplateResolver;
import org.thymeleaf.spring6.view.ThymeleafViewResolver;
import org.thymeleaf.templatemode.TemplateMode;

/**
 * Thymeleaf 設定
 */
@Configuration
public class ThymeleafConfig {

    private final ApplicationContext applicationContext;

    public ThymeleafConfig(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    /**
     * テンプレートリゾルバー設定
     */
    @Bean
    public SpringResourceTemplateResolver templateResolver() {
        SpringResourceTemplateResolver templateResolver = new SpringResourceTemplateResolver();
        templateResolver.setApplicationContext(applicationContext);
        templateResolver.setPrefix("classpath:/templates/");
        templateResolver.setSuffix(".html");
        templateResolver.setTemplateMode(TemplateMode.HTML);
        templateResolver.setCharacterEncoding("UTF-8");
        templateResolver.setCacheable(false);
        templateResolver.setOrder(1);
        templateResolver.setCheckExistence(true);
        return templateResolver;
    }

    /**
     * テンプレートエンジン設定
     */
    @Bean
    public SpringTemplateEngine templateEngine(SpringResourceTemplateResolver templateResolver) {
        SpringTemplateEngine templateEngine = new SpringTemplateEngine();
        templateEngine.setTemplateResolver(templateResolver);
        templateEngine.addDialect(new LayoutDialect());
        templateEngine.setEnableSpringELCompiler(true);
        return templateEngine;
    }

    /**
     * ビューリゾルバー設定
     */
    @Bean
    public ThymeleafViewResolver viewResolver(SpringTemplateEngine templateEngine) {
        ThymeleafViewResolver viewResolver = new ThymeleafViewResolver();
        viewResolver.setTemplateEngine(templateEngine);
        viewResolver.setCharacterEncoding("UTF-8");
        viewResolver.setContentType("text/html;charset=UTF-8");
        viewResolver.setOrder(1);
        return viewResolver;
    }
}
