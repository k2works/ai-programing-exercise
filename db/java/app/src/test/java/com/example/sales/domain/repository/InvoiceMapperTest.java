package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 請求データMapperのテストクラス
 */
class InvoiceMapperTest extends AbstractDatabaseTest {

    @Autowired
    private InvoiceMapper invoiceMapper;

    @Autowired
    private CompanyMapper companyMapper;

    @Autowired
    private CompanyGroupMapper companyGroupMapper;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    void setUp() {
        // テストデータをクリア（外部キー制約により、参照元を先に削除）
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
        // 請求を作成
        Invoice invoice = new Invoice();
        invoice.setInvoiceNo("I000000001");
        invoice.setInvoiceDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        invoice.setCompanyCode("00000001");
        invoice.setCustomerBranch(0);
        invoice.setLastReceivedAmount(0);
        invoice.setMonthSalesAmount(100000);
        invoice.setMonthReceivedAmount(0);
        invoice.setMonthInvoiceAmount(100000);
        invoice.setConsumptionTax(10000);
        invoice.setInvoiceReceivedAmount(0);
        invoice.setCreatedBy("tester");
        invoice.setUpdatedBy("tester");

        // 挿入
        int result = invoiceMapper.insert(invoice);
        assertThat(result).isEqualTo(1);

        // 取得
        Optional<Invoice> found = invoiceMapper.findById("I000000001");
        assertThat(found).isPresent();
        assertThat(found.get().getInvoiceNo()).isEqualTo("I000000001");
        assertThat(found.get().getCompanyCode()).isEqualTo("00000001");
        assertThat(found.get().getMonthInvoiceAmount()).isEqualTo(100000);
    }

    @Test
    void testUpdate() {
        // 請求を作成
        Invoice invoice = new Invoice();
        invoice.setInvoiceNo("I000000001");
        invoice.setInvoiceDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        invoice.setCompanyCode("00000001");
        invoice.setCustomerBranch(0);
        invoice.setLastReceivedAmount(0);
        invoice.setMonthSalesAmount(100000);
        invoice.setMonthReceivedAmount(0);
        invoice.setMonthInvoiceAmount(100000);
        invoice.setConsumptionTax(10000);
        invoice.setInvoiceReceivedAmount(0);
        invoice.setCreatedBy("tester");
        invoice.setUpdatedBy("tester");
        invoiceMapper.insert(invoice);

        // 更新
        invoice.setMonthReceivedAmount(50000);
        invoice.setMonthInvoiceAmount(50000);
        int updateResult = invoiceMapper.update(invoice);
        assertThat(updateResult).isEqualTo(1);

        // 確認
        Optional<Invoice> updated = invoiceMapper.findById("I000000001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getMonthReceivedAmount()).isEqualTo(50000);
        assertThat(updated.get().getMonthInvoiceAmount()).isEqualTo(50000);
    }

    @Test
    void testDelete() {
        // 請求を作成
        Invoice invoice = new Invoice();
        invoice.setInvoiceNo("I000000001");
        invoice.setInvoiceDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        invoice.setCompanyCode("00000001");
        invoice.setCustomerBranch(0);
        invoice.setMonthInvoiceAmount(100000);
        invoice.setCreatedBy("tester");
        invoice.setUpdatedBy("tester");
        invoiceMapper.insert(invoice);

        // 削除
        int deleteResult = invoiceMapper.delete("I000000001");
        assertThat(deleteResult).isEqualTo(1);

        // 確認
        Optional<Invoice> deleted = invoiceMapper.findById("I000000001");
        assertThat(deleted).isEmpty();
    }

    @Test
    void testFindByCompanyCode() {
        // 請求を2件作成
        Invoice invoice1 = new Invoice();
        invoice1.setInvoiceNo("I000000001");
        invoice1.setInvoiceDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        invoice1.setCompanyCode("00000001");
        invoice1.setCustomerBranch(0);
        invoice1.setMonthInvoiceAmount(100000);
        invoice1.setCreatedBy("tester");
        invoice1.setUpdatedBy("tester");
        invoiceMapper.insert(invoice1);

        Invoice invoice2 = new Invoice();
        invoice2.setInvoiceNo("I000000002");
        invoice2.setInvoiceDate(LocalDateTime.of(2024, 2, 29, 0, 0));
        invoice2.setCompanyCode("00000001");
        invoice2.setCustomerBranch(0);
        invoice2.setMonthInvoiceAmount(200000);
        invoice2.setCreatedBy("tester");
        invoice2.setUpdatedBy("tester");
        invoiceMapper.insert(invoice2);

        // 取引先コードで検索
        List<Invoice> invoices = invoiceMapper.findByCompanyCode("00000001");
        assertThat(invoices).hasSize(2);
    }

    @Test
    void testFindAll() {
        // 請求を2件作成
        Invoice invoice1 = new Invoice();
        invoice1.setInvoiceNo("I000000001");
        invoice1.setInvoiceDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        invoice1.setCompanyCode("00000001");
        invoice1.setMonthInvoiceAmount(100000);
        invoice1.setCreatedBy("tester");
        invoice1.setUpdatedBy("tester");
        invoiceMapper.insert(invoice1);

        Invoice invoice2 = new Invoice();
        invoice2.setInvoiceNo("I000000002");
        invoice2.setInvoiceDate(LocalDateTime.of(2024, 2, 29, 0, 0));
        invoice2.setCompanyCode("00000001");
        invoice2.setMonthInvoiceAmount(200000);
        invoice2.setCreatedBy("tester");
        invoice2.setUpdatedBy("tester");
        invoiceMapper.insert(invoice2);

        // 全件取得
        List<Invoice> invoices = invoiceMapper.findAll();
        assertThat(invoices).hasSize(2);
    }
}
