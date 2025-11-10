package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 与信残高データMapperのテストクラス
 */
class CreditBalanceMapperTest extends AbstractDatabaseTest {

    @Autowired
    private CreditBalanceMapper creditBalanceMapper;

    @Autowired
    private CompanyMapper companyMapper;

    @Autowired
    private CompanyGroupMapper companyGroupMapper;

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

        // 取引先グループを作成
        CompanyGroup companyGroup = new CompanyGroup();
        companyGroup.setCompanyGroupCode("0001");
        companyGroup.setCompanyGroupName("テストグループ");
        companyGroup.setCreatedBy("tester");
        companyGroup.setUpdatedBy("tester");
        companyGroupMapper.insert(companyGroup);

        // 取引先を作成
        Company company = new Company();
        company.setCompanyCode("00000001");
        company.setCompanyName("テスト取引先");
        company.setCompanyNameKana("テストトリヒキサキ");
        company.setSupplierType(0);
        company.setCompanyGroupCode("0001");
        company.setCreatedBy("tester");
        company.setUpdatedBy("tester");
        companyMapper.insert(company);
    }

    @Test
    void testInsertAndFindById() {
        // 与信残高を作成
        CreditBalance creditBalance = new CreditBalance();
        creditBalance.setCompanyCode("00000001");
        creditBalance.setOrderBalance(100000);
        creditBalance.setReceivableBalance(200000);
        creditBalance.setPayableBalance(50000);
        creditBalance.setCreatedBy("tester");
        creditBalance.setUpdatedBy("tester");

        // 挿入
        creditBalanceMapper.insert(creditBalance);

        // 取得
        Optional<CreditBalance> found = creditBalanceMapper.findById("00000001");
        assertThat(found).isPresent();
        assertThat(found.get().getCompanyCode()).isEqualTo("00000001");
        assertThat(found.get().getOrderBalance()).isEqualTo(100000);
        assertThat(found.get().getReceivableBalance()).isEqualTo(200000);
        assertThat(found.get().getPayableBalance()).isEqualTo(50000);
    }

    @Test
    void testUpdate() {
        // 与信残高を作成
        CreditBalance creditBalance = new CreditBalance();
        creditBalance.setCompanyCode("00000001");
        creditBalance.setOrderBalance(100000);
        creditBalance.setReceivableBalance(200000);
        creditBalance.setPayableBalance(50000);
        creditBalance.setCreatedBy("tester");
        creditBalance.setUpdatedBy("tester");
        creditBalanceMapper.insert(creditBalance);

        // 更新
        creditBalance.setOrderBalance(150000);
        creditBalance.setReceivableBalance(250000);
        creditBalanceMapper.update(creditBalance);

        // 確認
        Optional<CreditBalance> updated = creditBalanceMapper.findById("00000001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getOrderBalance()).isEqualTo(150000);
        assertThat(updated.get().getReceivableBalance()).isEqualTo(250000);
    }

    @Test
    void testDelete() {
        // 与信残高を作成
        CreditBalance creditBalance = new CreditBalance();
        creditBalance.setCompanyCode("00000001");
        creditBalance.setOrderBalance(100000);
        creditBalance.setReceivableBalance(200000);
        creditBalance.setPayableBalance(50000);
        creditBalance.setCreatedBy("tester");
        creditBalance.setUpdatedBy("tester");
        creditBalanceMapper.insert(creditBalance);

        // 削除
        creditBalanceMapper.delete("00000001");

        // 確認
        Optional<CreditBalance> deleted = creditBalanceMapper.findById("00000001");
        assertThat(deleted).isEmpty();
    }

    @Test
    void testFindAll() {
        // 取引先をもう1件追加
        Company company2 = new Company();
        company2.setCompanyCode("00000002");
        company2.setCompanyName("テスト取引先2");
        company2.setCompanyNameKana("テストトリヒキサキ2");
        company2.setSupplierType(0);
        company2.setCompanyGroupCode("0001");
        company2.setCreatedBy("tester");
        company2.setUpdatedBy("tester");
        companyMapper.insert(company2);

        // 与信残高を2件作成
        CreditBalance creditBalance1 = new CreditBalance();
        creditBalance1.setCompanyCode("00000001");
        creditBalance1.setOrderBalance(100000);
        creditBalance1.setReceivableBalance(200000);
        creditBalance1.setPayableBalance(50000);
        creditBalance1.setCreatedBy("tester");
        creditBalance1.setUpdatedBy("tester");
        creditBalanceMapper.insert(creditBalance1);

        CreditBalance creditBalance2 = new CreditBalance();
        creditBalance2.setCompanyCode("00000002");
        creditBalance2.setOrderBalance(150000);
        creditBalance2.setReceivableBalance(250000);
        creditBalance2.setPayableBalance(75000);
        creditBalance2.setCreatedBy("tester");
        creditBalance2.setUpdatedBy("tester");
        creditBalanceMapper.insert(creditBalance2);

        // 全件取得
        List<CreditBalance> creditBalances = creditBalanceMapper.findAll();
        assertThat(creditBalances).hasSize(2);
    }
}
