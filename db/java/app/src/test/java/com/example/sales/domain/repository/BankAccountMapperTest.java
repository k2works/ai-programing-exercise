package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.BankAccount;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 入金口座マスタMapperのテストクラス
 */
class BankAccountMapperTest extends AbstractDatabaseTest {

    @Autowired
    private BankAccountMapper bankAccountMapper;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    void setUp() {
        // テストデータをクリア（外部キー制約により、参照元を先に削除）
        // 第7章のテーブル
        jdbcTemplate.execute("DELETE FROM 与信残高データ");
        jdbcTemplate.execute("DELETE FROM 自動採番マスタ");

        // 第6章のテーブル
        jdbcTemplate.execute("DELETE FROM 請求データ明細");
        jdbcTemplate.execute("DELETE FROM 請求データ");
        jdbcTemplate.execute("DELETE FROM 入金データ");
        jdbcTemplate.execute("DELETE FROM 支払データ");
        jdbcTemplate.execute("DELETE FROM 入金口座マスタ");

        // 第5章のテーブル
        jdbcTemplate.execute("DELETE FROM 在庫データ");
        jdbcTemplate.execute("DELETE FROM 仕入データ明細");
        jdbcTemplate.execute("DELETE FROM 仕入データ");
        jdbcTemplate.execute("DELETE FROM 発注データ明細");
        jdbcTemplate.execute("DELETE FROM 発注データ");

        // 第4章のテーブル
        jdbcTemplate.execute("DELETE FROM 売上データ明細");
        jdbcTemplate.execute("DELETE FROM 売上データ");
        jdbcTemplate.execute("DELETE FROM 受注データ明細");
        jdbcTemplate.execute("DELETE FROM 受注データ");

        // 第3章以前のテーブル
        jdbcTemplate.execute("DELETE FROM 倉庫マスタ");
        jdbcTemplate.execute("DELETE FROM 顧客マスタ");
        jdbcTemplate.execute("DELETE FROM 仕入先マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先分類所属マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先グループマスタ");
        jdbcTemplate.execute("DELETE FROM 社員マスタ");
        jdbcTemplate.execute("DELETE FROM 部門マスタ");
    }

    @Test
    void testInsertAndFindById() {
        // 入金口座を作成
        BankAccount bankAccount = new BankAccount();
        bankAccount.setBankAccountCode("00000001");
        bankAccount.setAccountHolder("株式会社テスト");
        bankAccount.setBankName("テスト銀行");
        bankAccount.setBranchName("テスト支店");
        bankAccount.setAccountType(1);
        bankAccount.setAccountNumber("1234567");
        bankAccount.setCreatedBy("tester");
        bankAccount.setUpdatedBy("tester");

        // 挿入
        int result = bankAccountMapper.insert(bankAccount);
        assertThat(result).isEqualTo(1);

        // 取得
        Optional<BankAccount> found = bankAccountMapper.findById("00000001");
        assertThat(found).isPresent();
        assertThat(found.get().getBankAccountCode()).isEqualTo("00000001");
        assertThat(found.get().getAccountHolder()).isEqualTo("株式会社テスト");
        assertThat(found.get().getBankName()).isEqualTo("テスト銀行");
    }

    @Test
    void testUpdate() {
        // 入金口座を作成
        BankAccount bankAccount = new BankAccount();
        bankAccount.setBankAccountCode("00000001");
        bankAccount.setAccountHolder("株式会社テスト");
        bankAccount.setBankName("テスト銀行");
        bankAccount.setBranchName("テスト支店");
        bankAccount.setAccountType(1);
        bankAccount.setAccountNumber("1234567");
        bankAccount.setCreatedBy("tester");
        bankAccount.setUpdatedBy("tester");
        bankAccountMapper.insert(bankAccount);

        // 更新
        bankAccount.setAccountHolder("株式会社テスト更新");
        bankAccount.setBankName("更新銀行");
        int updateResult = bankAccountMapper.update(bankAccount);
        assertThat(updateResult).isEqualTo(1);

        // 確認
        Optional<BankAccount> updated = bankAccountMapper.findById("00000001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getAccountHolder()).isEqualTo("株式会社テスト更新");
        assertThat(updated.get().getBankName()).isEqualTo("更新銀行");
    }

    @Test
    void testDelete() {
        // 入金口座を作成
        BankAccount bankAccount = new BankAccount();
        bankAccount.setBankAccountCode("00000001");
        bankAccount.setAccountHolder("株式会社テスト");
        bankAccount.setBankName("テスト銀行");
        bankAccount.setBranchName("テスト支店");
        bankAccount.setAccountType(1);
        bankAccount.setAccountNumber("1234567");
        bankAccount.setCreatedBy("tester");
        bankAccount.setUpdatedBy("tester");
        bankAccountMapper.insert(bankAccount);

        // 削除
        int deleteResult = bankAccountMapper.delete("00000001");
        assertThat(deleteResult).isEqualTo(1);

        // 確認
        Optional<BankAccount> deleted = bankAccountMapper.findById("00000001");
        assertThat(deleted).isEmpty();
    }

    @Test
    void testFindAll() {
        // 入金口座を2件作成
        BankAccount bankAccount1 = new BankAccount();
        bankAccount1.setBankAccountCode("00000001");
        bankAccount1.setAccountHolder("株式会社テスト1");
        bankAccount1.setBankName("テスト銀行");
        bankAccount1.setCreatedBy("tester");
        bankAccount1.setUpdatedBy("tester");
        bankAccountMapper.insert(bankAccount1);

        BankAccount bankAccount2 = new BankAccount();
        bankAccount2.setBankAccountCode("00000002");
        bankAccount2.setAccountHolder("株式会社テスト2");
        bankAccount2.setBankName("テスト銀行");
        bankAccount2.setCreatedBy("tester");
        bankAccount2.setUpdatedBy("tester");
        bankAccountMapper.insert(bankAccount2);

        // 全件取得
        List<BankAccount> bankAccounts = bankAccountMapper.findAll();
        assertThat(bankAccounts).hasSize(2);
    }
}
