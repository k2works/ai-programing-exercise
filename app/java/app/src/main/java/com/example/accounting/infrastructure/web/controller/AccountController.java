package com.example.accounting.infrastructure.web.controller;

import com.example.accounting.application.port.in.AccountUseCase;
import com.example.accounting.domain.model.Account;
import com.example.accounting.infrastructure.web.dto.AccountRequest;
import com.example.accounting.infrastructure.web.dto.AccountResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "勘定科目", description = "勘定科目マスタ管理API")
@RestController
@RequestMapping("/api/v1/accounts")
public class AccountController {

    private final AccountUseCase accountUseCase;

    public AccountController(AccountUseCase accountUseCase) {
        this.accountUseCase = accountUseCase;
    }

    /**
     * すべての勘定科目を取得
     */
    @Operation(summary = "勘定科目一覧取得", description = "登録されているすべての勘定科目を取得します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "正常に取得",
                content = @Content(schema = @Schema(implementation = AccountResponse.class)))
    })
    @GetMapping
    public ResponseEntity<List<AccountResponse>> getAllAccounts() {
        List<Account> accounts = accountUseCase.getAllAccounts();
        List<AccountResponse> response = accounts.stream()
                .map(AccountResponse::from)
                .collect(Collectors.toList());
        return ResponseEntity.ok(response);
    }

    /**
     * 科目コードで勘定科目を取得
     */
    @Operation(summary = "勘定科目取得", description = "科目コードを指定して勘定科目を取得します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "正常に取得",
                content = @Content(schema = @Schema(implementation = AccountResponse.class))),
        @ApiResponse(responseCode = "404", description = "勘定科目が見つかりません")
    })
    @GetMapping("/{accountCode}")
    public ResponseEntity<AccountResponse> getAccount(
            @Parameter(description = "勘定科目コード", required = true)
            @PathVariable String accountCode) {
        Account account = accountUseCase.getAccountByCode(accountCode);
        return ResponseEntity.ok(AccountResponse.from(account));
    }

    /**
     * 新しい勘定科目を作成
     */
    @Operation(summary = "勘定科目作成", description = "新しい勘定科目を作成します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "201", description = "正常に作成",
                content = @Content(schema = @Schema(implementation = AccountResponse.class))),
        @ApiResponse(responseCode = "400", description = "入力値が不正です")
    })
    @PostMapping
    public ResponseEntity<AccountResponse> createAccount(
            @Parameter(description = "勘定科目情報", required = true)
            @Valid @RequestBody AccountRequest request) {
        Account account = request.toDomain();
        Account created = accountUseCase.createAccount(account);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(AccountResponse.from(created));
    }

    /**
     * 勘定科目を更新
     */
    @Operation(summary = "勘定科目更新", description = "既存の勘定科目を更新します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "正常に更新",
                content = @Content(schema = @Schema(implementation = AccountResponse.class))),
        @ApiResponse(responseCode = "400", description = "入力値が不正です"),
        @ApiResponse(responseCode = "404", description = "勘定科目が見つかりません")
    })
    @PutMapping("/{accountCode}")
    public ResponseEntity<AccountResponse> updateAccount(
            @Parameter(description = "勘定科目コード", required = true)
            @PathVariable String accountCode,
            @Parameter(description = "更新する勘定科目情報", required = true)
            @Valid @RequestBody AccountRequest request) {
        Account account = request.toDomain();
        Account updated = accountUseCase.updateAccount(accountCode, account);
        return ResponseEntity.ok(AccountResponse.from(updated));
    }

    /**
     * 勘定科目を削除
     */
    @Operation(summary = "勘定科目削除", description = "指定された勘定科目を削除します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "204", description = "正常に削除"),
        @ApiResponse(responseCode = "404", description = "勘定科目が見つかりません")
    })
    @DeleteMapping("/{accountCode}")
    public ResponseEntity<Void> deleteAccount(
            @Parameter(description = "勘定科目コード", required = true)
            @PathVariable String accountCode) {
        accountUseCase.deleteAccount(accountCode);
        return ResponseEntity.noContent().build();
    }
}
