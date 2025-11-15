package infrastructure.repository

import infrastructure.domain.{Sales, SalesDetail}
import scalikejdbc._

/**
 * 売上データのリポジトリ
 */
trait SalesRepository {
  def create(sales: Sales)(implicit session: DBSession): Int
  def findByNo(salesNo: String)(implicit session: DBSession): Option[Sales]
  def findAll()(implicit session: DBSession): List[Sales]
  def findByOrderNo(orderNo: String)(implicit session: DBSession): List[Sales]
  def update(sales: Sales)(implicit session: DBSession): Int
  def delete(salesNo: String)(implicit session: DBSession): Int
}

/**
 * SalesRepository の実装
 */
class SalesRepositoryImpl extends SalesRepository {

  override def create(sales: Sales)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 売上 (
        売上番号, 売上日, 売上区分, 受注番号,
        部門コード, 開始日, 取引先コード, 売上金額, 消費税,
        訂正番号, 元伝票番号,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${sales.salesNo}, ${sales.salesDate}, ${sales.salesType}, ${sales.orderNo},
        ${sales.deptCode}, ${sales.startDate}, ${sales.compCode}, ${sales.salesAmnt}, ${sales.cmpTax},
        ${sales.updatedNo}, ${sales.originalNo},
        ${sales.createDate}, ${sales.creator}, ${sales.updateDate}, ${sales.updater}
      )
    """.update.apply()
  }

  override def findByNo(salesNo: String)(implicit session: DBSession): Option[Sales] = {
    sql"""
      SELECT
        売上番号, 売上日, 売上区分, 受注番号,
        部門コード, 開始日, 取引先コード, 売上金額, 消費税,
        訂正番号, 元伝票番号,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 売上
      WHERE 売上番号 = $salesNo
    """.map(Sales.apply).single.apply()
  }

  override def findAll()(implicit session: DBSession): List[Sales] = {
    sql"""
      SELECT
        売上番号, 売上日, 売上区分, 受注番号,
        部門コード, 開始日, 取引先コード, 売上金額, 消費税,
        訂正番号, 元伝票番号,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 売上
      ORDER BY 売上日 DESC
    """.map(Sales.apply).list.apply()
  }

  override def findByOrderNo(orderNo: String)(implicit session: DBSession): List[Sales] = {
    sql"""
      SELECT
        売上番号, 売上日, 売上区分, 受注番号,
        部門コード, 開始日, 取引先コード, 売上金額, 消費税,
        訂正番号, 元伝票番号,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 売上
      WHERE 受注番号 = $orderNo
      ORDER BY 売上日
    """.map(Sales.apply).list.apply()
  }

  override def update(sales: Sales)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 売上 SET
        売上日 = ${sales.salesDate},
        売上区分 = ${sales.salesType},
        売上金額 = ${sales.salesAmnt},
        消費税 = ${sales.cmpTax},
        更新日時 = ${sales.updateDate},
        更新者名 = ${sales.updater}
      WHERE 売上番号 = ${sales.salesNo}
    """.update.apply()
  }

  override def delete(salesNo: String)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 売上
      WHERE 売上番号 = $salesNo
    """.update.apply()
  }
}

object SalesRepository {
  def apply(): SalesRepository = new SalesRepositoryImpl()
}

/**
 * 売上明細のリポジトリ
 */
trait SalesDetailRepository {
  def create(detail: SalesDetail)(implicit session: DBSession): Int
  def findBySalesNo(salesNo: String)(implicit session: DBSession): List[SalesDetail]
  def update(detail: SalesDetail)(implicit session: DBSession): Int
  def delete(salesNo: String, rowNo: Int)(implicit session: DBSession): Int
}

/**
 * SalesDetailRepository の実装
 */
class SalesDetailRepositoryImpl extends SalesDetailRepository {

  override def create(detail: SalesDetail)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 売上明細 (
        売上番号, 明細番号, 商品コード, 商品名, 販売単価,
        出荷済数量, 数量, 値引額, 請求日, 請求番号,
        請求遅延区分, 自動仕訳日,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${detail.salesNo}, ${detail.rowNo}, ${detail.prodCode}, ${detail.prodName}, ${detail.unitPrice},
        ${detail.deliveredQty}, ${detail.qty}, ${detail.discount}, ${detail.invoicedDate}, ${detail.invoiceNo},
        ${detail.invoiceDelayType}, ${detail.autoJournalDate},
        ${detail.createDate}, ${detail.creator}, ${detail.updateDate}, ${detail.updater}
      )
    """.update.apply()
  }

  override def findBySalesNo(salesNo: String)(implicit session: DBSession): List[SalesDetail] = {
    sql"""
      SELECT
        売上番号, 明細番号, 商品コード, 商品名, 販売単価,
        出荷済数量, 数量, 値引額, 請求日, 請求番号,
        請求遅延区分, 自動仕訳日,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 売上明細
      WHERE 売上番号 = $salesNo
      ORDER BY 明細番号
    """.map(SalesDetail.apply).list.apply()
  }

  override def update(detail: SalesDetail)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 売上明細 SET
        出荷済数量 = ${detail.deliveredQty},
        数量 = ${detail.qty},
        値引額 = ${detail.discount},
        請求日 = ${detail.invoicedDate},
        請求番号 = ${detail.invoiceNo},
        更新日時 = ${detail.updateDate},
        更新者名 = ${detail.updater}
      WHERE 売上番号 = ${detail.salesNo} AND 明細番号 = ${detail.rowNo}
    """.update.apply()
  }

  override def delete(salesNo: String, rowNo: Int)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 売上明細
      WHERE 売上番号 = $salesNo AND 明細番号 = $rowNo
    """.update.apply()
  }
}

object SalesDetailRepository {
  def apply(): SalesDetailRepository = new SalesDetailRepositoryImpl()
}
