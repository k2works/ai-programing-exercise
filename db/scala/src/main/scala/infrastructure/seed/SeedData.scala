package infrastructure.seed

import scalikejdbc._
import infrastructure.domain._
import infrastructure.repository._
import java.time.LocalDateTime

object SeedData {

  // データベースのクリア
  def truncateTables()(implicit session: DBSession): Unit = {
    // 外部キー制約の逆順でクリア
    sql"TRUNCATE TABLE 与信残高 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 自動採番 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 売上明細 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 売上 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 受注明細 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 受注 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 仕入明細 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 仕入 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 発注明細 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 発注 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 在庫 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 代替商品 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 顧客別販売単価 CASCADE".execute.apply()
    sql"TRUNCATE TABLE 商品マスタ CASCADE".execute.apply()
    sql"TRUNCATE TABLE 商品分類マスタ CASCADE".execute.apply()
    sql"TRUNCATE TABLE 仕入先マスタ CASCADE".execute.apply()
    sql"TRUNCATE TABLE 顧客マスタ CASCADE".execute.apply()
    sql"TRUNCATE TABLE 取引先マスタ CASCADE".execute.apply()
    sql"TRUNCATE TABLE 取引先グループマスタ CASCADE".execute.apply()
    sql"TRUNCATE TABLE 社員マスタ CASCADE".execute.apply()
    sql"TRUNCATE TABLE 部門マスタ CASCADE".execute.apply()
    sql"TRUNCATE TABLE 倉庫マスタ CASCADE".execute.apply()
  }

  // 部門マスタのシード
  def seedDepartments()(implicit session: DBSession): Int = {
    val now = LocalDateTime.now()
    val endDate = LocalDateTime.of(9999, 12, 31, 23, 59, 59)
    val repo = DepartmentRepository()

    val departments = List(
      // 本社
      Department("000000", now, endDate, "本社", 1, "/000000", 0, 1, now, "seed", now, "seed"),

      // 食肉製造・販売事業
      Department("100000", now, endDate, "食肉製造・販売事業", 2, "/000000/100000", 0, 0, now, "seed", now, "seed"),
      Department("110000", now, endDate, "食肉加工部門", 3, "/000000/100000/110000", 0, 0, now, "seed", now, "seed"),
      Department("111000", now, endDate, "牛肉・豚肉・鶏肉課", 4, "/000000/100000/110000/111000", 1, 1, now, "seed", now, "seed"),
      Department("112000", now, endDate, "食肉加工品課", 4, "/000000/100000/110000/112000", 1, 1, now, "seed", now, "seed"),

      Department("120000", now, endDate, "小売販売部門", 3, "/000000/100000/120000", 0, 0, now, "seed", now, "seed"),
      Department("121000", now, endDate, "直営小売店課", 4, "/000000/100000/120000/121000", 1, 1, now, "seed", now, "seed"),
      Department("122000", now, endDate, "百貨店・スーパー向け販売課", 4, "/000000/100000/120000/122000", 1, 1, now, "seed", now, "seed"),

      Department("130000", now, endDate, "新規取引先開拓部門", 3, "/000000/100000/130000", 0, 0, now, "seed", now, "seed"),
      Department("131000", now, endDate, "ホテル・旅館向け課", 4, "/000000/100000/130000/131000", 1, 1, now, "seed", now, "seed"),
      Department("132000", now, endDate, "飲食店向け課", 4, "/000000/100000/130000/132000", 1, 1, now, "seed", now, "seed"),

      // 食肉加工品事業
      Department("200000", now, endDate, "食肉加工品事業", 2, "/000000/200000", 0, 0, now, "seed", now, "seed"),
      Department("210000", now, endDate, "自社ブランド部門", 3, "/000000/200000/210000", 0, 0, now, "seed", now, "seed"),
      Department("211000", now, endDate, "贈答用製品製造課", 4, "/000000/200000/210000/211000", 1, 1, now, "seed", now, "seed"),
      Department("212000", now, endDate, "道の駅・土産物製品販売課", 4, "/000000/200000/210000/212000", 1, 1, now, "seed", now, "seed"),

      Department("220000", now, endDate, "OEM部門", 3, "/000000/200000/220000", 0, 0, now, "seed", now, "seed"),
      Department("221000", now, endDate, "客先要望対応課", 4, "/000000/200000/220000/221000", 1, 1, now, "seed", now, "seed"),

      // コンサルティング事業
      Department("300000", now, endDate, "コンサルティング事業", 2, "/000000/300000", 0, 0, now, "seed", now, "seed"),
      Department("310000", now, endDate, "顧客対応部門", 3, "/000000/300000/310000", 0, 0, now, "seed", now, "seed"),
      Department("311000", now, endDate, "メニュー提案課", 4, "/000000/300000/310000/311000", 1, 1, now, "seed", now, "seed"),
      Department("312000", now, endDate, "半加工商品提供課", 4, "/000000/300000/310000/312000", 1, 1, now, "seed", now, "seed")
    )

    departments.map(repo.create).sum
  }

  // 社員マスタのシード
  def seedEmployees()(implicit session: DBSession): Int = {
    val now = LocalDateTime.now()
    val startDate = LocalDateTime.of(2020, 4, 1, 0, 0, 0)
    val repo = EmployeeRepository()

    val employees = List(
      // 本社 - 経営層（2名）
      Employee("EMP000001", "山田太郎", "ヤマダタロウ", "password", "03-0001-0001", "03-0001-0001", "000000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000002", "佐藤次郎", "サトウジロウ", "password", "03-0001-0002", "03-0001-0002", "000000", startDate, "1", "1", now, "seed", now, "seed"),

      // 食肉製造・販売事業（17名）
      // 食肉加工部門
      Employee("EMP000003", "田中三郎", "タナカサブロウ", "password", "03-0001-0003", "03-0001-0003", "111000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000004", "鈴木四郎", "スズキシロウ", "password", "03-0001-0004", "03-0001-0004", "111000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000005", "高橋五郎", "タカハシゴロウ", "password", "03-0001-0005", "03-0001-0005", "111000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000006", "伊藤六郎", "イトウロクロウ", "password", "03-0001-0006", "03-0001-0006", "112000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000007", "渡辺七郎", "ワタナベシチロウ", "password", "03-0001-0007", "03-0001-0007", "112000", startDate, "2", "2", now, "seed", now, "seed"),

      // 小売販売部門
      Employee("EMP000008", "山本八郎", "ヤマモトハチロウ", "password", "03-0001-0008", "03-0001-0008", "121000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000009", "中村九郎", "ナカムラクロウ", "password", "03-0001-0009", "03-0001-0009", "121000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000010", "小林十郎", "コバヤシジュウロウ", "password", "03-0001-0010", "03-0001-0010", "121000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000011", "加藤一郎", "カトウイチロウ", "password", "03-0001-0011", "03-0001-0011", "122000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000012", "吉田二郎", "ヨシダジロウ", "password", "03-0001-0012", "03-0001-0012", "122000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000013", "山田花子", "ヤマダハナコ", "password", "03-0001-0013", "03-0001-0013", "122000", startDate, "2", "2", now, "seed", now, "seed"),

      // 新規取引先開拓部門
      Employee("EMP000014", "佐々木三郎", "ササキサブロウ", "password", "03-0001-0014", "03-0001-0014", "131000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000015", "山口四郎", "ヤマグチシロウ", "password", "03-0001-0015", "03-0001-0015", "131000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000016", "松本五郎", "マツモトゴロウ", "password", "03-0001-0016", "03-0001-0016", "131000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000017", "井上六郎", "イノウエロクロウ", "password", "03-0001-0017", "03-0001-0017", "132000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000018", "木村七郎", "キムラシチロウ", "password", "03-0001-0018", "03-0001-0018", "132000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000019", "林八郎", "ハヤシハチロウ", "password", "03-0001-0019", "03-0001-0019", "132000", startDate, "2", "2", now, "seed", now, "seed"),

      // 食肉加工品事業（9名）
      // 自社ブランド部門
      Employee("EMP000020", "斎藤九郎", "サイトウクロウ", "password", "03-0001-0020", "03-0001-0020", "211000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000021", "清水十郎", "シミズジュウロウ", "password", "03-0001-0021", "03-0001-0021", "211000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000022", "山崎一郎", "ヤマザキイチロウ", "password", "03-0001-0022", "03-0001-0022", "211000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000023", "森二郎", "モリジロウ", "password", "03-0001-0023", "03-0001-0023", "212000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000024", "池田三郎", "イケダサブロウ", "password", "03-0001-0024", "03-0001-0024", "212000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000025", "橋本四郎", "ハシモトシロウ", "password", "03-0001-0025", "03-0001-0025", "212000", startDate, "2", "2", now, "seed", now, "seed"),

      // OEM部門
      Employee("EMP000026", "石川五郎", "イシカワゴロウ", "password", "03-0001-0026", "03-0001-0026", "221000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000027", "前田六郎", "マエダロクロウ", "password", "03-0001-0027", "03-0001-0027", "221000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000028", "藤田七郎", "フジタシチロウ", "password", "03-0001-0028", "03-0001-0028", "221000", startDate, "2", "2", now, "seed", now, "seed"),

      // コンサルティング事業（6名）
      // 顧客対応部門
      Employee("EMP000029", "岡田八郎", "オカダハチロウ", "password", "03-0001-0029", "03-0001-0029", "311000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000030", "後藤九郎", "ゴトウクロウ", "password", "03-0001-0030", "03-0001-0030", "311000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000031", "長谷川十郎", "ハセガワジュウロウ", "password", "03-0001-0031", "03-0001-0031", "311000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000032", "村上一郎", "ムラカミイチロウ", "password", "03-0001-0032", "03-0001-0032", "312000", startDate, "1", "1", now, "seed", now, "seed"),
      Employee("EMP000033", "近藤二郎", "コンドウジロウ", "password", "03-0001-0033", "03-0001-0033", "312000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000034", "石井三郎", "イシイサブロウ", "password", "03-0001-0034", "03-0001-0034", "312000", startDate, "2", "2", now, "seed", now, "seed"),

      // パート社員（追加で11名、合計45名）
      Employee("EMP000035", "遠藤美咲", "エンドウミサキ", "password", "03-0001-0035", "03-0001-0035", "121000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000036", "青木由美", "アオキユミ", "password", "03-0001-0036", "03-0001-0036", "121000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000037", "坂本恵子", "サカモトケイコ", "password", "03-0001-0037", "03-0001-0037", "122000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000038", "福田真理", "フクダマリ", "password", "03-0001-0038", "03-0001-0038", "122000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000039", "西村さくら", "ニシムラサクラ", "password", "03-0001-0039", "03-0001-0039", "211000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000040", "藤井あゆみ", "フジイアユミ", "password", "03-0001-0040", "03-0001-0040", "211000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000041", "太田まゆみ", "オオタマユミ", "password", "03-0001-0041", "03-0001-0041", "212000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000042", "三浦めぐみ", "ミウラメグミ", "password", "03-0001-0042", "03-0001-0042", "212000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000043", "岡本ゆかり", "オカモトユカリ", "password", "03-0001-0043", "03-0001-0043", "311000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000044", "松田かおり", "マツダカオリ", "password", "03-0001-0044", "03-0001-0044", "311000", startDate, "2", "2", now, "seed", now, "seed"),
      Employee("EMP000045", "中島みどり", "ナカジマミドリ", "password", "03-0001-0045", "03-0001-0045", "312000", startDate, "2", "2", now, "seed", now, "seed")
    )

    employees.map(repo.create).sum
  }

  // 取引先グループマスタのシード
  def seedCompanyGroups()(implicit session: DBSession): Int = {
    val now = LocalDateTime.now()
    val repo = CompanyGroupRepository()

    val groups = List(
      CompanyGroup("G001", "百貨店グループ", now, "seed", now, "seed"),
      CompanyGroup("G002", "スーパーグループ", now, "seed", now, "seed"),
      CompanyGroup("G003", "ホテル・旅館グループ", now, "seed", now, "seed"),
      CompanyGroup("G004", "飲食店グループ", now, "seed", now, "seed"),
      CompanyGroup("G005", "観光施設グループ", now, "seed", now, "seed"),
      CompanyGroup("G006", "食肉卸グループ", now, "seed", now, "seed"),
      CompanyGroup("G007", "畜産業者グループ", now, "seed", now, "seed")
    )

    groups.map(repo.create).sum
  }

  // 取引先マスタのシード
  def seedCompanies()(implicit session: DBSession): Int = {
    val now = LocalDateTime.now()
    val repo = CompanyRepository()

    val companies = List(
      // 得意先
      Company("COMP0001", "地域百貨店", Some("チイキヒャッカテン"), 0, None, None, None, None, 0, 0, "G001", 50000000, 0, now, "seed", now, "seed"),
      Company("COMP0002", "X県有名百貨店", Some("ケンユウメイヒャッカテン"), 0, None, None, None, None, 0, 0, "G001", 80000000, 0, now, "seed", now, "seed"),
      Company("COMP0003", "地域スーパーチェーン", Some("チイキスーパー"), 0, None, None, None, None, 0, 0, "G002", 30000000, 0, now, "seed", now, "seed"),
      Company("COMP0004", "広域スーパーチェーン", Some("コウイキスーパー"), 0, None, None, None, None, 0, 0, "G002", 100000000, 0, now, "seed", now, "seed"),
      Company("COMP0005", "シティホテル", Some("シティホテル"), 0, None, None, None, None, 0, 0, "G003", 20000000, 0, now, "seed", now, "seed"),
      Company("COMP0006", "温泉旅館", Some("オンセンリョカン"), 0, None, None, None, None, 0, 0, "G003", 15000000, 0, now, "seed", now, "seed"),
      Company("COMP0007", "焼肉レストラン", Some("ヤキニクレストラン"), 0, None, None, None, None, 0, 0, "G004", 10000000, 0, now, "seed", now, "seed"),
      Company("COMP0008", "イタリアンレストラン", Some("イタリアンレストラン"), 0, None, None, None, None, 0, 0, "G004", 8000000, 0, now, "seed", now, "seed"),
      Company("COMP0009", "道の駅", Some("ミチノエキ"), 0, None, None, None, None, 0, 0, "G005", 5000000, 0, now, "seed", now, "seed"),
      Company("COMP0010", "観光センター", Some("カンコウセンター"), 0, None, None, None, None, 0, 0, "G005", 6000000, 0, now, "seed", now, "seed"),

      // 仕入先
      Company("COMP0011", "地域食肉卸A社", Some("チイキショクニクオロシA"), 1, None, None, None, None, 0, 0, "G006", 0, 0, now, "seed", now, "seed"),
      Company("COMP0012", "地域食肉卸B社", Some("チイキショクニクオロシB"), 1, None, None, None, None, 0, 0, "G006", 0, 0, now, "seed", now, "seed"),
      Company("COMP0013", "地域畜産農家", Some("チイキチクサンノウカ"), 1, None, None, None, None, 0, 0, "G007", 0, 0, now, "seed", now, "seed"),
      Company("COMP0014", "県内畜産組合", Some("ケンナイチクサンクミアイ"), 1, None, None, None, None, 0, 0, "G007", 0, 0, now, "seed", now, "seed")
    )

    companies.map(repo.create).sum
  }

  // 商品分類マスタのシード
  def seedProductCategories()(implicit session: DBSession): Int = {
    val now = LocalDateTime.now()
    val repo = ProductCategoryRepository()

    val categories = List(
      ProductCategory("CAT001", "牛肉", 1, "/CAT001", 1, now, "seed", now, "seed"),
      ProductCategory("CAT002", "豚肉", 1, "/CAT002", 1, now, "seed", now, "seed"),
      ProductCategory("CAT003", "鶏肉", 1, "/CAT003", 1, now, "seed", now, "seed"),
      ProductCategory("CAT004", "加工品", 1, "/CAT004", 1, now, "seed", now, "seed"),
      ProductCategory("CAT005", "その他", 1, "/CAT005", 1, now, "seed", now, "seed")
    )

    categories.map(repo.create).sum
  }

  // 商品マスタのシード
  def seedProducts()(implicit session: DBSession): Int = {
    val now = LocalDateTime.now()
    val repo = ProductRepository()

    val products = List(
      // 牛肉製品
      Product("PROD00001", "黒毛和牛サーロインステーキ 200g", "サーロイン", Some("クロゲワギュウサーロイン"), Some("1"), None, 5000, 3500, 3500, 1, Some("CAT001"), None, 1, None, Some("COMP0011"), None, now, "seed", now, "seed"),
      Product("PROD00002", "黒毛和牛ロース 100g", "ロース", Some("クロゲワギュウロース"), Some("1"), None, 3000, 2100, 2100, 1, Some("CAT001"), None, 1, None, Some("COMP0011"), None, now, "seed", now, "seed"),
      Product("PROD00003", "黒毛和牛カルビ 100g", "カルビ", Some("クロゲワギュウカルビ"), Some("1"), None, 2500, 1750, 1750, 1, Some("CAT001"), None, 1, None, Some("COMP0011"), None, now, "seed", now, "seed"),
      Product("PROD00004", "黒毛和牛ヒレ 100g", "ヒレ", Some("クロゲワギュウヒレ"), Some("1"), None, 4000, 2800, 2800, 1, Some("CAT001"), None, 1, None, Some("COMP0011"), None, now, "seed", now, "seed"),
      Product("PROD00005", "国産牛切り落とし 200g", "切り落とし", Some("コクサンギュウキリオトシ"), Some("1"), None, 1000, 700, 700, 1, Some("CAT001"), None, 1, None, Some("COMP0011"), None, now, "seed", now, "seed"),

      // 豚肉製品
      Product("PROD00006", "国産豚ロース 100g", "豚ロース", Some("コクサンブタロース"), Some("1"), None, 500, 350, 350, 1, Some("CAT002"), None, 1, None, Some("COMP0012"), None, now, "seed", now, "seed"),
      Product("PROD00007", "国産豚バラ 100g", "豚バラ", Some("コクサンブタバラ"), Some("1"), None, 400, 280, 280, 1, Some("CAT002"), None, 1, None, Some("COMP0012"), None, now, "seed", now, "seed"),
      Product("PROD00008", "国産豚ヒレ 100g", "豚ヒレ", Some("コクサンブタヒレ"), Some("1"), None, 600, 420, 420, 1, Some("CAT002"), None, 1, None, Some("COMP0012"), None, now, "seed", now, "seed"),
      Product("PROD00009", "国産豚コマ 200g", "豚コマ", Some("コクサンブタコマ"), Some("1"), None, 350, 245, 245, 1, Some("CAT002"), None, 1, None, Some("COMP0012"), None, now, "seed", now, "seed"),
      Product("PROD00010", "国産豚肩ロース 100g", "豚肩ロース", Some("コクサンブタカタロース"), Some("1"), None, 450, 315, 315, 1, Some("CAT002"), None, 1, None, Some("COMP0012"), None, now, "seed", now, "seed"),

      // 鶏肉製品
      Product("PROD00011", "国産鶏もも肉 100g", "鶏もも", Some("コクサントリモモ"), Some("1"), None, 250, 175, 175, 1, Some("CAT003"), None, 1, None, Some("COMP0013"), None, now, "seed", now, "seed"),
      Product("PROD00012", "国産鶏むね肉 100g", "鶏むね", Some("コクサントリムネ"), Some("1"), None, 150, 105, 105, 1, Some("CAT003"), None, 1, None, Some("COMP0013"), None, now, "seed", now, "seed"),
      Product("PROD00013", "国産手羽先 100g", "手羽先", Some("コクサンテバサキ"), Some("1"), None, 200, 140, 140, 1, Some("CAT003"), None, 1, None, Some("COMP0013"), None, now, "seed", now, "seed"),
      Product("PROD00014", "国産手羽元 100g", "手羽元", Some("コクサンテバモト"), Some("1"), None, 180, 126, 126, 1, Some("CAT003"), None, 1, None, Some("COMP0013"), None, now, "seed", now, "seed"),
      Product("PROD00015", "国産鶏ささみ 100g", "鶏ささみ", Some("コクサントリササミ"), Some("1"), None, 300, 210, 210, 1, Some("CAT003"), None, 1, None, Some("COMP0013"), None, now, "seed", now, "seed"),

      // 加工品
      Product("PROD00016", "自家製ローストビーフ 100g", "ローストビーフ", Some("ローストビーフ"), Some("1"), None, 1500, 1050, 1050, 1, Some("CAT004"), None, 1, None, Some("COMP0014"), None, now, "seed", now, "seed"),
      Product("PROD00017", "自家製ハム 100g", "ハム", Some("ハム"), Some("1"), None, 800, 560, 560, 1, Some("CAT004"), None, 1, None, Some("COMP0014"), None, now, "seed", now, "seed"),
      Product("PROD00018", "自家製ソーセージ 100g", "ソーセージ", Some("ソーセージ"), Some("1"), None, 700, 490, 490, 1, Some("CAT004"), None, 1, None, Some("COMP0014"), None, now, "seed", now, "seed"),
      Product("PROD00019", "自家製ベーコン 100g", "ベーコン", Some("ベーコン"), Some("1"), None, 900, 630, 630, 1, Some("CAT004"), None, 1, None, Some("COMP0014"), None, now, "seed", now, "seed"),
      Product("PROD00020", "揚げたてコロッケ 1個", "コロッケ", Some("コロッケ"), Some("1"), None, 100, 70, 70, 1, Some("CAT004"), None, 1, None, Some("COMP0014"), None, now, "seed", now, "seed")
    )

    products.map(repo.create).sum
  }

  // 倉庫マスタのシード
  def seedWarehouses()(implicit session: DBSession): Int = {
    val now = LocalDateTime.now()
    val repo = WarehouseRepository()

    val warehouses = List(
      Warehouse("W01", "本社倉庫", Some("100-0001"), Some("東京都"), Some("千代田区千代田1-1-1"), None, Some("03-1234-5678"), None, now, "seed", now, "seed"),
      Warehouse("W02", "工場倉庫", Some("200-0001"), Some("神奈川県"), Some("横浜市中区山下町1-1-1"), None, Some("045-1234-5678"), None, now, "seed", now, "seed")
    )

    warehouses.map(repo.create).sum
  }

  // すべてのシードデータを投入
  def seedAll()(implicit session: DBSession): Unit = {
    println("🗑️  既存データをクリアしています...")
    truncateTables()
    println("✓ データベースをクリアしました")

    println("\n📊 シードデータを投入しています...")

    val deptCount = seedDepartments()
    println(s"✓ 部門マスタ: ${deptCount}件")

    val empCount = seedEmployees()
    println(s"✓ 社員マスタ: ${empCount}件")

    val grpCount = seedCompanyGroups()
    println(s"✓ 取引先グループマスタ: ${grpCount}件")

    val compCount = seedCompanies()
    println(s"✓ 取引先マスタ: ${compCount}件")

    val catCount = seedProductCategories()
    println(s"✓ 商品分類マスタ: ${catCount}件")

    val prodCount = seedProducts()
    println(s"✓ 商品マスタ: ${prodCount}件")

    val whCount = seedWarehouses()
    println(s"✓ 倉庫マスタ: ${whCount}件")

    println("\n🎉 シードデータの投入が完了しました！")
  }
}
