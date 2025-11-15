package infrastructure.repository

import infrastructure.domain.Stock
import scalikejdbc._

/**
 * 在庫データのリポジトリ
 */
trait StockRepository {
  def create(stock: Stock)(implicit session: DBSession): Int
  def findById(whCode: String, prodCode: String, rotNo: String, stockType: String, qualityType: String)(implicit session: DBSession): Option[Stock]
  def findByWarehouse(whCode: String)(implicit session: DBSession): List[Stock]
  def findByProduct(prodCode: String)(implicit session: DBSession): List[Stock]
  def update(stock: Stock)(implicit session: DBSession): Int
  def delete(whCode: String, prodCode: String, rotNo: String, stockType: String, qualityType: String)(implicit session: DBSession): Int
}

/**
 * StockRepository の実装
 */
class StockRepositoryImpl extends StockRepository {

  override def create(stock: Stock)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 在庫 (
        倉庫コード, 商品コード, ロット番号, 在庫区分, 品質区分,
        実在庫数, 有効在庫数, 最終出荷日,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${stock.whCode}, ${stock.prodCode}, ${stock.rotNo},
        ${stock.stockType}, ${stock.qualityType},
        ${stock.actual}, ${stock.valid}, ${stock.lastDeliveryDate},
        ${stock.createDate}, ${stock.creator}, ${stock.updateDate}, ${stock.updater}
      )
    """.update.apply()
  }

  override def findById(
    whCode: String,
    prodCode: String,
    rotNo: String,
    stockType: String,
    qualityType: String
  )(implicit session: DBSession): Option[Stock] = {
    sql"""
      SELECT
        倉庫コード, 商品コード, ロット番号, 在庫区分, 品質区分,
        実在庫数, 有効在庫数, 最終出荷日,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 在庫
      WHERE 倉庫コード = $whCode
        AND 商品コード = $prodCode
        AND ロット番号 = $rotNo
        AND 在庫区分 = $stockType
        AND 品質区分 = $qualityType
    """.map(Stock.apply).single.apply()
  }

  override def findByWarehouse(whCode: String)(implicit session: DBSession): List[Stock] = {
    sql"""
      SELECT
        倉庫コード, 商品コード, ロット番号, 在庫区分, 品質区分,
        実在庫数, 有効在庫数, 最終出荷日,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 在庫
      WHERE 倉庫コード = $whCode
      ORDER BY 商品コード, ロット番号
    """.map(Stock.apply).list.apply()
  }

  override def findByProduct(prodCode: String)(implicit session: DBSession): List[Stock] = {
    sql"""
      SELECT
        倉庫コード, 商品コード, ロット番号, 在庫区分, 品質区分,
        実在庫数, 有効在庫数, 最終出荷日,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 在庫
      WHERE 商品コード = $prodCode
      ORDER BY 倉庫コード, ロット番号
    """.map(Stock.apply).list.apply()
  }

  override def update(stock: Stock)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 在庫
      SET 実在庫数 = ${stock.actual},
          有効在庫数 = ${stock.valid},
          最終出荷日 = ${stock.lastDeliveryDate},
          更新日時 = ${stock.updateDate},
          更新者名 = ${stock.updater}
      WHERE 倉庫コード = ${stock.whCode}
        AND 商品コード = ${stock.prodCode}
        AND ロット番号 = ${stock.rotNo}
        AND 在庫区分 = ${stock.stockType}
        AND 品質区分 = ${stock.qualityType}
    """.update.apply()
  }

  override def delete(
    whCode: String,
    prodCode: String,
    rotNo: String,
    stockType: String,
    qualityType: String
  )(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 在庫
      WHERE 倉庫コード = $whCode
        AND 商品コード = $prodCode
        AND ロット番号 = $rotNo
        AND 在庫区分 = $stockType
        AND 品質区分 = $qualityType
    """.update.apply()
  }
}

object StockRepository {
  def apply(): StockRepository = new StockRepositoryImpl()
}
