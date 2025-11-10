package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.AutoNumber;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 自動採番マスタMapperのテストクラス
 */
class AutoNumberMapperTest extends AbstractDatabaseTest {

    @Autowired
    private AutoNumberMapper autoNumberMapper;

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
        // 自動採番を作成
        LocalDateTime yearMonth = LocalDateTime.of(2024, 1, 1, 0, 0);
        AutoNumber autoNumber = new AutoNumber();
        autoNumber.setSlipType("01");
        autoNumber.setYearMonth(yearMonth);
        autoNumber.setLastSlipNo(100);

        // 挿入
        autoNumberMapper.insert(autoNumber);

        // 取得
        Optional<AutoNumber> found = autoNumberMapper.findById("01", yearMonth);
        assertThat(found).isPresent();
        assertThat(found.get().getSlipType()).isEqualTo("01");
        assertThat(found.get().getYearMonth()).isEqualTo(yearMonth);
        assertThat(found.get().getLastSlipNo()).isEqualTo(100);
    }

    @Test
    void testUpdate() {
        // 自動採番を作成
        LocalDateTime yearMonth = LocalDateTime.of(2024, 1, 1, 0, 0);
        AutoNumber autoNumber = new AutoNumber();
        autoNumber.setSlipType("01");
        autoNumber.setYearMonth(yearMonth);
        autoNumber.setLastSlipNo(100);
        autoNumberMapper.insert(autoNumber);

        // 更新
        autoNumber.setLastSlipNo(200);
        autoNumberMapper.update(autoNumber);

        // 確認
        Optional<AutoNumber> updated = autoNumberMapper.findById("01", yearMonth);
        assertThat(updated).isPresent();
        assertThat(updated.get().getLastSlipNo()).isEqualTo(200);
    }

    @Test
    void testDelete() {
        // 自動採番を作成
        LocalDateTime yearMonth = LocalDateTime.of(2024, 1, 1, 0, 0);
        AutoNumber autoNumber = new AutoNumber();
        autoNumber.setSlipType("01");
        autoNumber.setYearMonth(yearMonth);
        autoNumber.setLastSlipNo(100);
        autoNumberMapper.insert(autoNumber);

        // 削除
        autoNumberMapper.delete("01", yearMonth);

        // 確認
        Optional<AutoNumber> deleted = autoNumberMapper.findById("01", yearMonth);
        assertThat(deleted).isEmpty();
    }

    @Test
    void testFindAll() {
        // 自動採番を2件作成
        LocalDateTime yearMonth1 = LocalDateTime.of(2024, 1, 1, 0, 0);
        AutoNumber autoNumber1 = new AutoNumber();
        autoNumber1.setSlipType("01");
        autoNumber1.setYearMonth(yearMonth1);
        autoNumber1.setLastSlipNo(100);
        autoNumberMapper.insert(autoNumber1);

        LocalDateTime yearMonth2 = LocalDateTime.of(2024, 2, 1, 0, 0);
        AutoNumber autoNumber2 = new AutoNumber();
        autoNumber2.setSlipType("02");
        autoNumber2.setYearMonth(yearMonth2);
        autoNumber2.setLastSlipNo(200);
        autoNumberMapper.insert(autoNumber2);

        // 全件取得
        List<AutoNumber> autoNumbers = autoNumberMapper.findAll();
        assertThat(autoNumbers).hasSize(2);
    }
}
