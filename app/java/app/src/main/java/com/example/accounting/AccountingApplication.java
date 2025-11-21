package com.example.accounting;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * 財務会計システム Spring Boot アプリケーション
 */
@SpringBootApplication
@MapperScan("com.example.accounting.mapper")
public class AccountingApplication {

    public static void main(String[] args) {
        SpringApplication.run(AccountingApplication.class, args);
    }
}
