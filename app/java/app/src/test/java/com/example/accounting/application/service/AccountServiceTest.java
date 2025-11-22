package com.example.accounting.application.service;

import com.example.accounting.TestDatabaseConfig;
import com.example.accounting.application.exception.AccountNotFoundException;
import com.example.accounting.application.port.out.AccountRepository;
import com.example.accounting.domain.model.Account;
import com.example.accounting.infrastructure.out.persistence.adapter.AccountAdapter;
import com.example.accounting.infrastructure.out.persistence.mapper.AccountMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 勘定科目サービスの統合テスト
 */
@DisplayName("勘定科目サービス - 統合テスト")
class AccountServiceTest extends TestDatabaseConfig {

    private AccountService accountService;
    private AccountMapper mapper;

    @BeforeEach
    void setUpEach() {
        // MyBatis Mapper の取得
        mapper = sqlSessionFactory.openSession(true).getMapper(AccountMapper.class);

        // Repository と Service の初期化
        AccountRepository repository = new AccountAdapter(mapper);
        accountService = new AccountService(repository);
    }

    @AfterEach
    void cleanup() throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            conn.createStatement().execute("DELETE FROM \"勘定科目マスタ\"");
        }
    }

    @Test
    @DisplayName("新規勘定科目を作成できる")
    void testCreateAccount() {
        // Given: 新規勘定科目
        Account account = createAccount("1010", "現金", "資産", "B", "借", "1");

        // When: 勘定科目を作成
        Account created = accountService.createAccount(account);

        // Then: 正しく作成されている
        assertThat(created).isNotNull();
        assertThat(created.getAccountCode()).isEqualTo("1010");
        assertThat(created.getAccountName()).isEqualTo("現金");
        assertThat(created.getBsplType()).isEqualTo("B");
    }

    @Test
    @DisplayName("重複する科目コードで作成するとエラー")
    void testCreateAccountWithDuplicateCode() {
        // Given: 既存の勘定科目
        Account account = createAccount("1010", "現金", "資産", "B", "借", "1");
        accountService.createAccount(account);

        // When/Then: 同じ科目コードで作成すると例外
        Account duplicate = createAccount("1010", "別の科目", "資産", "B", "借", "1");
        assertThatThrownBy(() -> accountService.createAccount(duplicate))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("既に存在します");
    }

    @Test
    @DisplayName("全勘定科目を取得できる")
    void testGetAllAccounts() {
        // Given: 複数の勘定科目
        accountService.createAccount(createAccount("1010", "現金", "資産", "B", "借", "1"));
        accountService.createAccount(createAccount("1020", "普通預金", "資産", "B", "借", "1"));
        accountService.createAccount(createAccount("2010", "買掛金", "負債", "B", "貸", "2"));

        // When: 全勘定科目を取得
        List<Account> accounts = accountService.getAllAccounts();

        // Then: 3件取得できる
        assertThat(accounts).hasSize(3);
        assertThat(accounts)
                .extracting(Account::getAccountCode)
                .containsExactlyInAnyOrder("1010", "1020", "2010");
    }

    @Test
    @DisplayName("科目コードで勘定科目を取得できる")
    void testGetAccountByCode() {
        // Given: 勘定科目
        accountService.createAccount(createAccount("1010", "現金", "資産", "B", "借", "1"));

        // When: 科目コードで取得
        Account account = accountService.getAccountByCode("1010");

        // Then: 正しく取得できる
        assertThat(account).isNotNull();
        assertThat(account.getAccountCode()).isEqualTo("1010");
        assertThat(account.getAccountName()).isEqualTo("現金");
    }

    @Test
    @DisplayName("存在しない科目コードで取得するとエラー")
    void testGetAccountByCodeNotFound() {
        // When/Then: 存在しない科目コードで取得すると例外
        assertThatThrownBy(() -> accountService.getAccountByCode("9999"))
                .isInstanceOf(AccountNotFoundException.class)
                .hasMessageContaining("が見つかりません");
    }

    @Test
    @DisplayName("BSPL区分で勘定科目を取得できる")
    void testGetAccountsByBsplType() {
        // Given: BS科目とPL科目
        accountService.createAccount(createAccount("1010", "現金", "資産", "B", "借", "1"));
        accountService.createAccount(createAccount("2010", "買掛金", "負債", "B", "貸", "2"));
        accountService.createAccount(createAccount("4010", "売上高", "収益", "P", "貸", "4"));

        // When: BS科目を取得
        List<Account> bsAccounts = accountService.getAccountsByBsplType("B");

        // Then: BS科目のみ取得できる
        assertThat(bsAccounts).hasSize(2);
        assertThat(bsAccounts)
                .extracting(Account::getAccountCode)
                .containsExactlyInAnyOrder("1010", "2010");
    }

    @Test
    @DisplayName("不正なBSPL区分で取得するとエラー")
    void testGetAccountsByInvalidBsplType() {
        // When/Then: 不正なBSPL区分で取得すると例外
        assertThatThrownBy(() -> accountService.getAccountsByBsplType("X"))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("'B' または 'P'");
    }

    @Test
    @DisplayName("勘定科目を更新できる")
    void testUpdateAccount() {
        // Given: 既存の勘定科目
        Account original = createAccount("1010", "現金", "資産", "B", "借", "1");
        accountService.createAccount(original);

        // When: 科目名を変更
        Account updated = createAccount("1010", "現金及び預金", "資産", "B", "借", "1");
        Account result = accountService.updateAccount("1010", updated);

        // Then: 更新されている
        assertThat(result.getAccountName()).isEqualTo("現金及び預金");

        Account fetched = accountService.getAccountByCode("1010");
        assertThat(fetched.getAccountName()).isEqualTo("現金及び預金");
    }

    @Test
    @DisplayName("科目コードを変更しようとするとエラー")
    void testUpdateAccountCodeChange() {
        // Given: 既存の勘定科目
        Account original = createAccount("1010", "現金", "資産", "B", "借", "1");
        accountService.createAccount(original);

        // When/Then: 科目コードを変更しようとすると例外
        Account updated = createAccount("1020", "現金", "資産", "B", "借", "1");
        assertThatThrownBy(() -> accountService.updateAccount("1010", updated))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("変更できません");
    }

    @Test
    @DisplayName("勘定科目を削除できる")
    void testDeleteAccount() {
        // Given: 既存の勘定科目
        Account account = createAccount("1010", "現金", "資産", "B", "借", "1");
        accountService.createAccount(account);

        // When: 勘定科目を削除
        accountService.deleteAccount("1010");

        // Then: 削除されている
        assertThatThrownBy(() -> accountService.getAccountByCode("1010"))
                .isInstanceOf(AccountNotFoundException.class);
    }

    @Test
    @DisplayName("存在しない勘定科目を削除しようとするとエラー")
    void testDeleteAccountNotFound() {
        // When/Then: 存在しない勘定科目を削除しようとすると例外
        assertThatThrownBy(() -> accountService.deleteAccount("9999"))
                .isInstanceOf(AccountNotFoundException.class)
                .hasMessageContaining("が見つかりません");
    }

    /**
     * テストデータ作成ヘルパー
     */
    private Account createAccount(String code, String name, String accountType,
                                   String bsplType, String debitCreditType, String elementType) {
        Account account = new Account();
        account.setAccountCode(code);
        account.setAccountName(name);
        account.setAccountAbbr(name);
        account.setAccountKana(name);
        account.setBsplType(bsplType);
        account.setDebitCreditType(debitCreditType);
        account.setElementType(elementType);
        account.setAggregationType("1");
        account.setDisplayOrder(100);
        return account;
    }
}
