package api.service

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._
import scalikejdbc._
import infrastructure.entity.Product
import infrastructure.repository.ProductRepository
import api.schema._
import java.time.LocalDateTime

class ProductServiceSpec extends AnyFlatSpec with Matchers with MockitoSugar {

  behavior of "ProductService"

  it should "商品を作成できる" in {
    val mockRepo = mock[ProductRepository]
    val service  = new ProductService(mockRepo)

    val request = CreateProductRequest(
      prodCode = "TEST00001",
      fullName = "テスト商品フルネーム",
      name = "テスト商品",
      kana = Some("テストショウヒン"),
      unitPrice = 1000,
      poPrice = 700,
      supCode = "COMP0011",
      categoryCode = None,
    )

    val now            = LocalDateTime.now()
    val createdProduct = Product(
      prodCode = request.prodCode,
      fullName = request.fullName,
      name = request.name,
      kana = request.kana,
      unitPrice = request.unitPrice,
      poPrice = request.poPrice,
      supCode = Some(request.supCode),
      categoryCode = request.categoryCode,
      createDate = now,
      creator = "api",
      updateDate = now,
      updater = "api",
    )

    // モックの設定
    when(mockRepo.create(any[Product])(any[DBSession])).thenReturn(1)
    when(mockRepo.findById(anyString())(any[DBSession])).thenReturn(Some(createdProduct))

    // 実行
    implicit val session: DBSession = AutoSession
    val result                      = service.createProduct(request)

    // 検証
    result.isRight shouldBe true
    result.toOption.get.prodCode shouldBe "TEST00001"
  }

  it should "販売単価が原価より低い場合はエラーを返す" in {
    val mockRepo = mock[ProductRepository]
    val service  = new ProductService(mockRepo)

    val request = CreateProductRequest(
      prodCode = "TEST00001",
      fullName = "テスト商品フルネーム",
      name = "テスト商品",
      kana = Some("テストショウヒン"),
      unitPrice = 500, // 原価より低い
      poPrice = 700,
      supCode = "COMP0011",
      categoryCode = None,
    )

    implicit val session: DBSession = AutoSession
    val result                      = service.createProduct(request)

    result.isLeft shouldBe true
    result.left.toOption.get should include("販売単価が仕入価格より低い")
  }

  it should "すべての商品を取得できる" in {
    val mockRepo = mock[ProductRepository]
    val service  = new ProductService(mockRepo)

    val now      = LocalDateTime.now()
    val products = List(
      Product(
        prodCode = "TEST00001",
        fullName = "商品1",
        name = "商品1",
        kana = Some("ショウヒン1"),
        unitPrice = 1000,
        poPrice = 700,
        supCode = Some("COMP0011"),
        categoryCode = None,
        createDate = now,
        creator = "test",
        updateDate = now,
        updater = "test",
      ),
      Product(
        prodCode = "TEST00002",
        fullName = "商品2",
        name = "商品2",
        kana = Some("ショウヒン2"),
        unitPrice = 2000,
        poPrice = 1400,
        supCode = Some("COMP0011"),
        categoryCode = None,
        createDate = now,
        creator = "test",
        updateDate = now,
        updater = "test",
      ),
    )

    when(mockRepo.findAll()(any[DBSession])).thenReturn(products)

    implicit val session: DBSession = AutoSession
    val result                      = service.getAllProducts()

    result.length shouldBe 2
  }
}
