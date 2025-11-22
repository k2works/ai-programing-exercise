package com.example.management.infrastructure.adapter.out.external;

import com.example.management.domain.FinancialData;
import com.example.management.domain.Journal;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.List;

@Component
public class FinancialAccountingClient {

    private final WebClient webClient;

    public FinancialAccountingClient(
            WebClient.Builder webClientBuilder,
            @Value("${financial-accounting-service.base-url:http://financial-accounting-service:8081}") String baseUrl) {
        this.webClient = webClientBuilder
            .baseUrl(baseUrl)
            .build();
    }

    public FinancialData fetchFinancialDataByFiscalYear(Integer fiscalYear) {
        List<Journal> journals = webClient.get()
            .uri("/api/journals?fiscalYear={fiscalYear}", fiscalYear)
            .retrieve()
            .bodyToFlux(Journal.class)
            .collectList()
            .block();

        return convertToFinancialData(fiscalYear, journals);
    }

    private FinancialData convertToFinancialData(Integer fiscalYear, List<Journal> journals) {
        // 財務会計データを管理会計ドメインモデルに変換（ACL）
        return new FinancialData(fiscalYear, journals);
    }
}
