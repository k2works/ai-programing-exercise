package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.entity.{Department, Employee}
import scalikejdbc._

import java.time.LocalDateTime

class EmployeeRepositorySpec extends DatabaseSpec {

  /**
   * テスト用の部門データを作成
   */
  private def setupTestDepartment(
    deptCode: String = "11101"
  )(implicit session: DBSession): Department = {
    val testDept = Department(
      deptCode = deptCode,
      startDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      endDate = LocalDateTime.of(2100, 12, 31, 0, 0, 0),
      name = "テスト部署",
      layer = 1,
      path = s"D$deptCode",
      lowestType = 1,
      slitYn = 0,
      createDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      updater = "admin",
    )

    val deptRepo = DepartmentRepository()
    deptRepo.create(testDept)
    testDept
  }

  "EmployeeRepository" should "社員を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = EmployeeRepository()

    // テスト用部門を作成
    DB localTx { implicit session => setupTestDepartment() }

    val testEmp = Employee(
      empCode = "E0001",
      name = "山田太郎",
      kana = "ヤマダタロウ",
      loginPassword = "pass1234",
      tel = "03-1234-5678",
      fax = "03-1234-5679",
      deptCode = "11101",
      startDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      occuCode = "01",
      approvalCode = "A1",
      createDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      updater = "admin",
    )

    // データを登録
    val insertResult = DB localTx { implicit session => repo.create(testEmp) }

    insertResult shouldBe 1

    // 取得して検証
    val result = DB readOnly { implicit session => repo.findById(testEmp.empCode) }

    result shouldBe defined
    result.get.empCode shouldBe testEmp.empCode
    result.get.name shouldBe testEmp.name
    result.get.deptCode shouldBe testEmp.deptCode
  }

  it should "すべての社員を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = EmployeeRepository()

    // テスト用部門を作成
    DB localTx { implicit session => setupTestDepartment("11102") }

    val testEmp = Employee(
      empCode = "E0002",
      name = "山田太郎",
      kana = "ヤマダタロウ",
      loginPassword = "pass1234",
      tel = "03-1234-5678",
      fax = "03-1234-5679",
      deptCode = "11102",
      startDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      occuCode = "01",
      approvalCode = "A1",
      createDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      updater = "admin",
    )

    DB localTx { implicit session => repo.create(testEmp) }

    val results = DB readOnly { implicit session => repo.findAll() }

    results should not be empty
    results.map(_.empCode) should contain("E0002")
  }

  it should "部門コードで社員を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = EmployeeRepository()

    // テスト用部門を作成
    DB localTx { implicit session => setupTestDepartment("11103") }

    val testEmp1 = Employee(
      empCode = "E0003",
      name = "山田太郎",
      kana = "ヤマダタロウ",
      loginPassword = "pass1234",
      tel = "03-1234-5678",
      fax = "03-1234-5679",
      deptCode = "11103",
      startDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      occuCode = "01",
      approvalCode = "A1",
      createDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      updater = "admin",
    )

    val testEmp2 = Employee(
      empCode = "E0004",
      name = "佐藤花子",
      kana = "サトウハナコ",
      loginPassword = "pass5678",
      tel = "03-2234-5678",
      fax = "03-2234-5679",
      deptCode = "11103",
      startDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      occuCode = "02",
      approvalCode = "A2",
      createDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      updater = "admin",
    )

    DB localTx { implicit session =>
      repo.create(testEmp1)
      repo.create(testEmp2)
    }

    val results = DB readOnly { implicit session => repo.findByDepartment("11103") }

    results should have size 2
    results.map(_.empCode) should contain allOf ("E0003", "E0004")
  }

  it should "社員を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = EmployeeRepository()

    // テスト用部門を作成
    DB localTx { implicit session => setupTestDepartment("11104") }

    val testEmp = Employee(
      empCode = "E0005",
      name = "山田太郎",
      kana = "ヤマダタロウ",
      loginPassword = "pass1234",
      tel = "03-1234-5678",
      fax = "03-1234-5679",
      deptCode = "11104",
      startDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      occuCode = "01",
      approvalCode = "A1",
      createDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      updater = "admin",
    )

    DB localTx { implicit session => repo.create(testEmp) }

    // 名前と電話番号を更新
    val updatedEmp = testEmp.copy(
      name = "山田一郎",
      tel = "03-9999-9999",
      updateDate = LocalDateTime.now(),
    )

    val updateResult = DB localTx { implicit session => repo.update(updatedEmp) }

    updateResult shouldBe 1

    // 更新されたことを確認
    val result = DB readOnly { implicit session => repo.findById(testEmp.empCode) }

    result shouldBe defined
    result.get.name shouldBe "山田一郎"
    result.get.tel shouldBe "03-9999-9999"
  }

  it should "社員を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = EmployeeRepository()

    // テスト用部門を作成
    DB localTx { implicit session => setupTestDepartment("11105") }

    val testEmp = Employee(
      empCode = "E0006",
      name = "山田太郎",
      kana = "ヤマダタロウ",
      loginPassword = "pass1234",
      tel = "03-1234-5678",
      fax = "03-1234-5679",
      deptCode = "11105",
      startDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      occuCode = "01",
      approvalCode = "A1",
      createDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      updater = "admin",
    )

    DB localTx { implicit session => repo.create(testEmp) }

    // 削除
    val deleteResult = DB localTx { implicit session => repo.delete(testEmp.empCode) }

    deleteResult shouldBe 1

    // 削除されたことを確認
    val result = DB readOnly { implicit session => repo.findById(testEmp.empCode) }

    result shouldBe None
  }

  it should "存在しない部門コードでは社員を登録できない" in withContainers { container =>
    setupWithMigrations(container)

    val repo = EmployeeRepository()

    val testEmp = Employee(
      empCode = "E0007",
      name = "山田太郎",
      kana = "ヤマダタロウ",
      loginPassword = "pass1234",
      tel = "03-1234-5678",
      fax = "03-1234-5679",
      deptCode = "99999", // 存在しない部門コード
      startDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      occuCode = "01",
      approvalCode = "A1",
      createDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 4, 1, 0, 0, 0),
      updater = "admin",
    )

    // 外部キー制約違反でエラーになることを確認
    an[java.sql.SQLException] should be thrownBy {
      DB localTx { implicit session => repo.create(testEmp) }
    }
  }
}
