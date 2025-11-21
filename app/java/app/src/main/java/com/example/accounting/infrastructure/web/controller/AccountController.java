package com.example.accounting.infrastructure.web.controller;

import com.example.accounting.application.service.AccountService;
import com.example.accounting.application.model.Account;
import com.example.accounting.infrastructure.web.dto.AccountRequest;
import com.example.accounting.infrastructure.web.dto.AccountResponse;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.stream.Collectors;

/**
 * 勘定科目 REST API コントローラー（Input Adapter）
 */
@RestController
@RequestMapping("/api/v1/accounts")
public class AccountController {

    private final AccountService accountService;

    public AccountController(AccountService accountService) {
        this.accountService = accountService;
    }

    /**
     * すべての勘定科目を取得
     */
    @GetMapping
    public ResponseEntity<List<AccountResponse>> getAllAccounts() {
        List<Account> accounts = accountService.getAllAccounts();
        List<AccountResponse> response = accounts.stream()
                .map(AccountResponse::from)
                .collect(Collectors.toList());
        return ResponseEntity.ok(response);
    }

    /**
     * 科目コードで勘定科目を取得
     */
    @GetMapping("/{accountCode}")
    public ResponseEntity<AccountResponse> getAccount(
            @PathVariable String accountCode) {
        Account account = accountService.getAccountByCode(accountCode);
        return ResponseEntity.ok(AccountResponse.from(account));
    }

    /**
     * 新しい勘定科目を作成
     */
    @PostMapping
    public ResponseEntity<AccountResponse> createAccount(
            @Valid @RequestBody AccountRequest request) {
        Account account = request.toDomain();
        Account created = accountService.createAccount(account);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(AccountResponse.from(created));
    }

    /**
     * 勘定科目を更新
     */
    @PutMapping("/{accountCode}")
    public ResponseEntity<AccountResponse> updateAccount(
            @PathVariable String accountCode,
            @Valid @RequestBody AccountRequest request) {
        Account account = request.toDomain();
        Account updated = accountService.updateAccount(accountCode, account);
        return ResponseEntity.ok(AccountResponse.from(updated));
    }

    /**
     * 勘定科目を削除
     */
    @DeleteMapping("/{accountCode}")
    public ResponseEntity<Void> deleteAccount(
            @PathVariable String accountCode) {
        accountService.deleteAccount(accountCode);
        return ResponseEntity.noContent().build();
    }
}
